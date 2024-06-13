package scala.today

import ox.*
import ox.either.*
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.client4.quick.*
import sttp.tapir.json.upickle.*
import sttp.tapir.generic.auto.*
import db.ProjectWithLatestReleaseRepo

import com.augustnagro.magnum.*
import javax.sql.DataSource

import upickle.default.*
import scala.today.db.Votes
import scala.today.db.ProjectWithLatestRelease
import java.time.OffsetDateTime

class Http(config: Config, ds: DataSource, projectsRepo: ProjectWithLatestReleaseRepo, search: Search):
  import Http.*

  def index = endpoint.get
    .in("")
    .in(query[Option[Int]]("page"))
    .out(htmlBodyUtf8)
    .handle { pageOpt =>
      either:
        connect(ds):
          val page = pageOpt.getOrElse(1) - 1

          val totalPages = projectsRepo
            .getPageCountMainQuery(pageSize)
            .logErrorDiscard("Failed to fetch page count")
            .ok()

          val projects = projectsRepo
            .fetchNRecentlyReleasedProjectsWithArtifacts(pageSize, page)
            .logErrorDiscard("Failed to fetch projects")
            .ok()

          val projectsAndArtifacts = Templates.ProjectAndArtifacts.fromDb(projects)

          Templates.index(projectsAndArtifacts, totalPages, page + 1).render
    }

  def paging = endpoint.get
    .in("paging")
    .in(query[Int]("page"))
    .out(htmlBodyUtf8)
    .handle { pageParam =>
      val page = pageParam - 1
      either:
        connect(ds):
          scribe.info(s"Fetching page $page with page size $pageSize")
          val totalPages = projectsRepo
            .getPageCountMainQuery(pageSize)
            .logErrorDiscard("Failed to fetch page count")
            .ok()

          val projects = projectsRepo
            .fetchNRecentlyReleasedProjectsWithArtifacts(pageSize, page)
            .logErrorDiscard("Failed to fetch projects")
            .ok()

          val projectsAndArtifacts = Templates.ProjectAndArtifacts.fromDb(projects)

          val pagingNav = Templates.oobPagingNavigation(page + 1, totalPages).render
          val table     = Templates.projectsTable(projectsAndArtifacts).render

          // out-of-bound swap of navigation + table
          s"""
             |$pagingNav
             |$table
             |""".stripMargin
    }

  def searchEndpoint = endpoint.get
    .in("search")
    .in(query[String]("q"))
    .in(query[Option[Int]]("page"))
    .in(header[Option[Boolean]]("HX-Request"))
    .out(htmlBodyUtf8)
    .handle { case (rawQuery, maybePage, hxRequestMaybe) =>
      scribe.info(s"Searching for '$rawQuery' with page $maybePage, query is from htmx: $hxRequestMaybe")
      either:
        connect(ds):
          val page                 = maybePage.getOrElse(1) - 1
          val searchResult         = search.runSearch(rawQuery, page).discardError.ok()
          val projectsAndArtifacts = Templates.ProjectAndArtifacts.fromDb(searchResult.projects)

          hxRequestMaybe match
            case Some(_) =>
              val pagingNav = Templates.oobPagingNavigation(page + 1, searchResult.totalPages).render
              val table     = Templates.projectsTable(projectsAndArtifacts).render

              // out-of-bound swap of navigation + table
              s"""
                |$pagingNav
                |$table
                |""".stripMargin

            case None =>
              // not from htmx, handle it like index
              Templates.index(projectsAndArtifacts, searchResult.totalPages, page + 1).render
    }

  case class Vote(id: String) derives ReadWriter

  def vote = endpoint.post
    .in("vote")
    .in(jsonBody[Vote])
    .handle { vote =>
      scribe.info(s"Handling vote for ${vote.id}")
      either:
        connect(ds):
          Votes
            .storeVote(vote.id)
            .logErrorDiscard("Failed to store a vote")
            .ok()
    }

  def serveForever: Unit =
    scribe.info(s"Starting server at port ${config.port}...")
    NettySyncServer()
      .host("0.0.0.0")
      .port(config.port)
      .addEndpoint(staticResourcesGetServerEndpoint("static")(classOf[App].getClassLoader(), "static/"))
      .addEndpoint(paging)
      .addEndpoint(searchEndpoint)
      .addEndpoint(vote)
      .addEndpoint(index)
      .startAndWait()
end Http

object Http:
  val pageSize = 10

class Search(projectsRepo: ProjectWithLatestReleaseRepo, pageSize: Int):
  import Search.*

  def runSearch(query: String, page: Int)(using DbCon): Either[Throwable, SearchResult] =
    Query.parse(query) match
      case Query.Plain(q)                          => searchPlain(q, page)
      case Query.Artifact(_, groupId, artifactId)  => searchArtifact(groupId, artifactId, page)
      case Query.Project(_, organization, project) => searchProject(organization, project, page)

  def searchPlain(query: String, page: Int)(using DbCon): Either[Throwable, SearchResult] = either:
    val totalPagesForQuery = projectsRepo
      .getPageCountQuery(query, pageSize)
      .logError("Failed to fetch page count")
      .ok()

    val projects = projectsRepo
      .searchForProjectsWithArtifacts(query, pageSize, page)
      .logError("Failed to search projects")
      .ok()

    SearchResult(totalPagesForQuery, projects)

  def searchArtifact(groupId: String, artifactId: String, page: Int)(using DbCon): Either[Throwable, SearchResult] = either:
    val totalPagesForQuery = projectsRepo
      .getPageCountArtifactQuery(groupId, artifactId, pageSize)
      .logError("Failed to fetch page count")
      .ok()

    val projects = projectsRepo
      .searchForArtifact(groupId, artifactId, pageSize, page)
      .logError("Failed to search projects")
      .ok()

    SearchResult(totalPagesForQuery, projects)

  def searchProject(organization: String, project: String, page: Int)(using DbCon): Either[Throwable, SearchResult] = either:
    val totalPagesForQuery = projectsRepo
      .getPageCountProjectQuery(organization, project, pageSize)
      .logError("Failed to fetch page count")
      .ok()

    val projects = projectsRepo
      .searchForProject(organization, project, pageSize, page)
      .logError("Failed to search projects")
      .ok()

    SearchResult(totalPagesForQuery, projects)

object Search:

  case class SearchResult(totalPages: Int, projects: Vector[(ProjectWithLatestRelease, OffsetDateTime)])

  enum Query(raw: String):
    case Plain(query: String) extends Query(query)
    case Artifact(raw: String, groupId: String, artifactId: String) extends Query(raw)
    case Project(raw: String, organization: String, project: String) extends Query(raw)

  object Query:
    def parse(raw: String): Query =
      if raw.contains("/") then
        raw.split("/").toVector match
          case Vector(organization, project) => Project(raw, organization, project)
          case _                             => Plain(raw)
      else if raw.contains(":") || raw.contains("::") || raw.contains(":::") then
        raw.replace(":::", "::").replace("::", ":").split(":").toVector match
          case Vector(groupId, artifactId) => Artifact(raw, groupId, artifactId)
          case _                           => Plain(raw)
      else Plain(raw)
