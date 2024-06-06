package scala.today

import sttp.model.*
import sttp.client4.quick.*
import sttp.client4.upicklejson.default.*
import sttp.client4.Response
import upickle.default.*
import scala.util.*
import ox.*
import ox.either.*
import ox.resilience.*
import com.augustnagro.magnum.*
import java.time.*
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import sttp.client4.SttpClientException.ReadException

case class Project(organization: String, repository: String) derives ReadWriter

case class Artifact(groupId: String, artifactId: String, version: String) derives ReadWriter

case class ArtifactInfo(
  groupId: String,
  artifactId: String,
  version: String,
  artifactName: String,
  project: String,
  releaseDate: Long,
  licenses: Vector[String],
  language: String,
  platform: String
) derives ReadWriter

case class ScrapingHttpException(url: Uri, response: Response[String], cause: Throwable)
    extends Exception(s"Failed to scrape $url\n${response.body}", cause)

class Scraper(baseUrl: Uri):
  val counter = AtomicInteger(0)

  def totalReqCount: Int = counter.get()

  def getProjects(): Either[Throwable, Vector[Project]] =
    handleGoaways:
      either:
        val response = catching(quickRequest.get(uri"$baseUrl/api/projects").send()).ok()
        counter.incrementAndGet()
        catching(read[Vector[Project]](response.body)).left
          .map { e =>
            ScrapingHttpException(uri"$baseUrl/api/projects", response, e)
          }
          .ok()

  def fetchArtifactsForProject(project: Project): Either[Throwable, Vector[Artifact]] =
    handleGoaways:
      either:
        val response = catching(quickRequest.get(uri"$baseUrl/api/projects/${project.organization}/${project.repository}/artifacts").send()).ok()
        counter.incrementAndGet()
        catching(read[Vector[Artifact]](response.body)).left
          .map { e =>
            ScrapingHttpException(uri"$baseUrl/api/projects/${project.organization}/${project.repository}/artifacts", response, e)
          }
          .ok()

  def fetchArtifactInfo(artifact: Artifact): Either[Throwable, ArtifactInfo] =
    handleGoaways:
      either:
        val response =
          catching(quickRequest.get(uri"$baseUrl/api/artifacts/${artifact.groupId}/${artifact.artifactId}/${artifact.version}").send()).ok()
        counter.incrementAndGet()
        catching(read[ArtifactInfo](response.body)).left
          .map { e =>
            ScrapingHttpException(uri"$baseUrl/api/artifacts/${artifact.groupId}/${artifact.artifactId}/${artifact.version}", response, e)
          }
          .ok()

  private def handleGoaways[A](block: => Either[Throwable, A]): Either[Throwable, A] =
    retryEither[Throwable, A](
      RetryPolicy(
        Schedule.Immediate(3),
        ResultPolicy.retryWhen {
          case re: ReadException if re.getCause().getMessage().contains("GOAWAY received") => true
          case _                                                                           => false
        }
      )
    )(block)

end Scraper

def runScrapingOncePerDay(config: Config): Unit =
  import java.time.*, temporal.ChronoUnit
  val startTime = Instant.now()

  scribe.info(s"Starting scraping job at $startTime...")
  scrapingJob(config)

  val dayAfterStart   = startTime.plus(1, ChronoUnit.DAYS)
  val now             = Instant.now()
  val jobTook         = Duration.between(startTime, now)
  val timeToWait      = dayAfterStart.toEpochMilli() - now.toEpochMilli()
  val nextStartTime   = Instant.ofEpochMilli(timeToWait + now.toEpochMilli())
  val durationToStart = Duration.between(now, nextStartTime)

  scribe.info(s"Scraping job took $jobTook")

  if timeToWait > 0 then
    scribe.info(s"Waiting for $durationToStart to start next scraping run...")
    Thread.sleep(timeToWait)

def scrapingJob(config: Config): Unit =
  catching(scrape(uri"https://index-dev.scala-lang.org", config.jdbcUrl)).flatten match
    case Left(err) =>
      scribe.error(s"Scraping failed", err)
    case Right(_) =>

case class MainViewRejectionException(msg: String, project: String, artifactName: String, version: String, cause: Throwable)
    extends Exception(msg, cause)

def scrape(scaladexApiUri: Uri, jdbcUrl: String): Either[Throwable, Unit] = either:
  val scraper                      = Scraper(scaladexApiUri)
  val projectRepo                  = db.ProjectRepo()
  val artifactRepo                 = db.ArtifactRepo()
  val projectWithLatestReleaseRepo = db.ProjectWithLatestReleaseRepo()
  val ds                           = db.createDatasource(jdbcUrl, "postgresml", "postgresml").ok()

  val projects = retryEither(RetryPolicy.backoff(3, 100.millis, 30.seconds, Jitter.Equal))(
    scraper.getProjects()
  ).ok()

  for project <- projects do
    val projectRow = transact(ds):
      projectRepo.upsert(db.ProjectCreator(project.organization, project.repository)).ok()

    val artifacts =
      retryEither(RetryPolicy.backoff(3, 100.millis, 30.seconds, Jitter.Equal))(
        scraper.fetchArtifactsForProject(project)
      ).ok()

    val artifactInfos = artifacts.mapPar(20) { artifact =>
      val info =
        retryEither(RetryPolicy.backoff(3, 100.millis, 30.seconds, Jitter.Equal))(
          scraper.fetchArtifactInfo(artifact)
        ).ok()

      artifact -> info
    }

    // produce the main view table entries
    artifactInfos.groupBy(_._2.artifactName).foreach { case (name, artsWithInfos) =>
      val result = either:
        val infosWithSemver =
          artsWithInfos.map { case (art, info) =>
            val version =
              Version
                .parseTolerant(art.version)
                .left
                .map { e =>
                  val stackTrace = e.printToString

                  MainViewRejectionException(
                    s"Failed to parse version ${art.version} for ${project.organization}/${project.repository} - $name\n$stackTrace",
                    s"${project.organization}/${project.repository}",
                    name,
                    art.version,
                    e
                  )
                }
                .ok()

            (version, art, info)
          }

        val (latestVersion, art, info) =
          infosWithSemver
            .sortBy(_._1)
            .last

        val artifactsForTheLatestVersion =
          infosWithSemver.filter(_._1 == latestVersion)

        val projectWithLatestRelease =
          artifactsForTheLatestVersion.foldLeft(
            db.ProjectWithLatestRelease(
              projectId = projectRow.id,
              organization = project.organization,
              repository = project.repository,
              groupId = art.groupId,
              lastVersion = latestVersion.original,
              artifactName = name,
              project = info.project,
              releaseDate = OffsetDateTime.ofInstant(Instant.ofEpochMilli(info.releaseDate), ZoneOffset.UTC),
              licenses = IArray.empty,
              language = IArray.empty,
              platform = IArray.empty
            )
          ) { case (acc, (semver, art, info)) =>
            acc.copy(
              releaseDate =
                if info.releaseDate > acc.releaseDate.toInstant().toEpochMilli() then
                  OffsetDateTime.ofInstant(Instant.ofEpochMilli(info.releaseDate), ZoneOffset.UTC)
                else acc.releaseDate,
              licenses = (acc.licenses ++ IArray.from(info.licenses)).distinct,
              language = (acc.language ++ IArray(info.language)).distinct,
              platform = (acc.platform ++ IArray(info.platform)).distinct
            )
          }

        transact(ds):
          projectWithLatestReleaseRepo.upsert(projectWithLatestRelease).ok()

        scribe.info(s"Upserted project with latest release for ${project.organization}/${project.repository} - version: ${latestVersion.original}")

      result match
        case Left(e: MainViewRejectionException) =>
          connect(ds):
            db.RejectedArtifact.store(e.msg, e.project, e.artifactName, e.version).ok()
          scribe.error(e.msg, e)
        case Left(e) =>
          scribe.error(s"Failed to upsert project with latest release for ${project.organization}/${project.repository} - $name", e)
        case Right(_) => ()
    }

    val artifactCreators = artifactInfos.map { case (art, ai) =>
      db.ArtifactCreator(
        groupId = ai.groupId,
        artifactId = ai.artifactId,
        version = ai.version,
        artifactName = ai.artifactName,
        project = ai.project,
        projectFk = projectRow.id,
        releaseDate = OffsetDateTime.ofInstant(Instant.ofEpochMilli(ai.releaseDate), ZoneOffset.UTC),
        licenses = IArray.from(ai.licenses),
        language = ai.language,
        platform = ai.platform
      )
    }

    val artifactRows = artifactCreators.mapPar(20) { ac =>
      transact(ds):
        artifactRepo.upsert(ac).ok()
    }

    scribe.info(s"Upserted ${artifactRows.size} artifacts for project ${project.organization}/${project.repository} id: ${projectRow.id}")
