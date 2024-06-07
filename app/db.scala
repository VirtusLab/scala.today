package scala.today

import scala.util.*
import com.augustnagro.magnum.*
import com.augustnagro.magnum.pg.PgCodec.given
import com.zaxxer.hikari.HikariDataSource
import org.flywaydb.core.api.output.MigrateResult
import org.flywaydb.core.Flyway
import java.time.OffsetDateTime
import org.flywaydb.core.internal.plugin.PluginRegister

object db:

  def createDatasource(jdbcUrl: String, user: String, password: String): Either[Throwable, HikariDataSource] =
    Try {
      val ds = HikariDataSource()
      ds.setJdbcUrl(jdbcUrl)
      ds.setUsername(user)
      ds.setPassword(password)
      ds
    }.toEither

  def performMigrations(jdbcUrl: String, user: String, password: String): Either[Throwable, MigrateResult] =
    Try {
      val flyway = Flyway
        .configure()
        .baselineVersion("0")
        .baselineOnMigrate(true)
        .dataSource(jdbcUrl, user, password)
        .locations("filesystem:./resources/sql")
        .load()

      flyway.migrate()
    }.toEither

  case class ProjectCreator(organization: String, repository: String) derives DbCodec

  @SqlName("projects")
  @Table(PostgresDbType, SqlNameMapper.SameCase)
  case class ProjectRow(@Id id: Int, organization: String, repository: String) derives DbCodec

  class ProjectRepo extends Repo[ProjectCreator, ProjectRow, Int]:
    // TODO run() is unsafe, handle errors
    def upsert(pc: ProjectCreator)(using DbTx): Either[Throwable, ProjectRow] =
      sql"""INSERT INTO projects (organization, repository)
            VALUES (${pc.organization}, ${pc.repository}) 
            ON CONFLICT DO NOTHING RETURNING *""".returning[ProjectRow].run().headOption match
        case None =>
          sql"SELECT * FROM projects WHERE organization = ${pc.organization} AND repository = ${pc.repository}"
            .query[ProjectRow]
            .run()
            .headOption match
            case None        => Left(Exception("Failed to find upserted project"))
            case Some(value) => Right(value)
        case Some(value) => Right(value)

  case class ArtifactCreator(
    groupId: String,
    artifactId: String,
    version: String,
    artifactName: String,
    project: String,
    projectFk: Int,
    releaseDate: OffsetDateTime,
    licenses: IArray[String],
    language: String,
    platform: String
  ) derives DbCodec

  @SqlName("artifacts")
  @Table(PostgresDbType, SqlNameMapper.SameCase)
  case class ArtifactRow(
    @Id id: Int,
    groupId: String,
    artifactId: String,
    version: String,
    artifactName: String,
    project: String,
    projectFk: Int,
    releaseDate: OffsetDateTime,
    licenses: IArray[String],
    language: String,
    platform: String
  ) derives DbCodec

  class ArtifactRepo extends Repo[ArtifactCreator, ArtifactRow, Int]:
    // TODO run() is unsafe, handle errors
    def upsert(ac: ArtifactCreator)(using DbTx): Either[Throwable, ArtifactRow] =
      sql"""INSERT INTO artifacts (groupId, artifactId, version, artifactName, project, projectFk, releaseDate, licenses, language, platform)
            VALUES (${ac.groupId}, ${ac.artifactId}, ${ac.version}, ${ac.artifactName}, ${ac.project}, ${ac.projectFk}, ${ac.releaseDate}, ${ac.licenses}, ${ac.language}, ${ac.platform})
            ON CONFLICT DO NOTHING RETURNING *""".returning[ArtifactRow].run().headOption match
        case None =>
          sql"SELECT * FROM artifacts WHERE groupId = ${ac.groupId} AND artifactId = ${ac.artifactId} AND version = ${ac.version}"
            .query[ArtifactRow]
            .run()
            .headOption match
            case None        => Left(Exception("Failed to find upserted artifact"))
            case Some(value) => Right(value)
        case Some(value) => Right(value)

  @SqlName("projects_with_latest_release")
  @Table(PostgresDbType, SqlNameMapper.SameCase)
  case class ProjectWithLatestRelease(
    @Id projectId: Int,
    organization: String,
    repository: String,
    groupId: String,
    lastVersion: String,
    artifactName: String,
    project: String,
    releaseDate: OffsetDateTime,
    licenses: IArray[String],
    language: IArray[String],
    platform: IArray[String]
  ) derives DbCodec

  object ProjectWithLatestRelease:
    val T = TableInfo[ProjectWithLatestRelease, ProjectWithLatestRelease, Int]

  class ProjectWithLatestReleaseRepo extends Repo[ProjectWithLatestRelease, ProjectWithLatestRelease, Int]:
    import ProjectWithLatestRelease.T

    // TODO run() is unsafe, handle errors
    def upsert(p: ProjectWithLatestRelease)(using DbTx): Either[Throwable, ProjectWithLatestRelease] =
      sql"""INSERT INTO $T (${T.projectId}, ${T.organization}, ${T.repository}, ${T.groupId}, ${T.lastVersion}, ${T.artifactName}, ${T.project}, ${T.releaseDate}, ${T.licenses}, ${T.language}, ${T.platform})
            VALUES (${p.projectId}, ${p.organization}, ${p.repository}, ${p.groupId}, ${p.lastVersion}, ${p.artifactName}, ${p.project}, ${p.releaseDate}, ${p.licenses}, ${p.language}, ${p.platform})
            ON CONFLICT (${T.organization}, ${T.repository}, ${T.artifactName}) DO UPDATE SET
              ${T.projectId} = EXCLUDED.${T.projectId},
              ${T.groupId} = EXCLUDED.${T.groupId},
              ${T.lastVersion} = EXCLUDED.${T.lastVersion},
              ${T.project} = EXCLUDED.${T.project},
              ${T.releaseDate} = EXCLUDED.${T.releaseDate},
              ${T.licenses} = EXCLUDED.${T.licenses},
              ${T.language} = EXCLUDED.${T.language},
              ${T.platform} = EXCLUDED.${T.platform}
            RETURNING *""".returning[ProjectWithLatestRelease].run().headOption match
        case None =>
          sql"SELECT * FROM $T WHERE ${T.organization} = ${p.organization} AND ${T.repository} = ${p.repository} AND ${T.artifactName} = ${p.artifactName}"
            .query[ProjectWithLatestRelease]
            .run()
            .headOption match
            case None        => Left(Exception("Failed to find upserted project with latest release"))
            case Some(value) => Right(value)
        case Some(value) => Right(value)

    def getPageCountMainQuery(pageSize: Int)(using DbCon): Either[Throwable, Int] =
      sql"""SELECT COUNT(*) FROM (
              SELECT ${T.organization}, ${T.repository}
              FROM $T
              GROUP BY ${T.organization}, ${T.repository}
            ) AS t""".query[Int].run().headOption match
        case None                         => Left(Exception("Failed to get page count"))
        case Some(value) if pageSize == 0 => Right(0)
        case Some(value)                  => Right(value / pageSize)

    def fetchNRecentlyReleasedProjectsWithArtifacts(pageSize: Int, page: Int = 0)(using
      DbCon
    ): Either[Throwable, Vector[(ProjectWithLatestRelease, OffsetDateTime)]] =
      val pwlr = T.alias("pwlr")
      val rp   = T.alias("rp")
      sql"""SELECT ${pwlr.projectId}, ${pwlr.organization}, ${pwlr.repository}, ${pwlr.groupId}, ${pwlr.lastVersion}, ${pwlr.artifactName}, ${pwlr.project}, ${pwlr.releaseDate}, ${pwlr.licenses}, ${pwlr.language}, ${pwlr.platform}, rp.latestReleaseDate FROM (
              SELECT ${T.organization}, ${T.repository}, max(${T.releaseDate}) AS latestReleaseDate
              FROM $T 
              GROUP BY ${T.organization}, ${T.repository}
              ORDER BY latestReleaseDate DESC
              OFFSET ${page * pageSize} LIMIT $pageSize
            ) rp
            JOIN $pwlr
            ON ${rp.organization} = ${pwlr.organization} AND ${rp.repository} = ${pwlr.repository}"""
        .query[(ProjectWithLatestRelease, OffsetDateTime)]
        .run()
        .attempt

  object RejectedArtifact:
    def store(msg: String, project: String, artifactName: String, version: String)(using DbCon): Either[Throwable, Unit] =
      sql"""INSERT INTO failed_main_view_artifacts (msg, project, artifactName, version)
            VALUES ($msg, $project, $artifactName, $version)""".update.run().attempt
