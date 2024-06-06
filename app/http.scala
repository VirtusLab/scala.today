package scala.today

import ox.*
import ox.either.*
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.client4.quick.*
import db.ProjectWithLatestReleaseRepo

import com.augustnagro.magnum.*

import org.flywaydb.core.Flyway
import com.zaxxer.hikari.*
import javax.sql.DataSource

class Http(config: Config, ds: DataSource, repo: ProjectWithLatestReleaseRepo):

  // todo make configurable by user prolly (as a param)
  val pageSize = 10

  def index = endpoint.get
    .in("")
    .in(query[Option[Int]]("page"))
    .out(htmlBodyUtf8)
    .handle { pageOpt =>
      either:
        connect(ds):
          val page = pageOpt.getOrElse(1)

          val totalPages = repo
            .getPageCountMainQuery(pageSize)
            .left
            .map { t =>
              scribe.error("Failed to fetch page count", t)
            }
            .ok()

          val projects = repo
            .fetchNRecentlyReleasedProjectsWithArtifacts(pageSize, page)
            .left
            .map { t =>
              scribe.error("Failed to fetch projects", t)
            }
            .ok()

          val projectsAndArtifacts = Templates.ProjectAndArtifacts.fromDb(projects)

          Templates.index(projectsAndArtifacts, totalPages, page).render
    }

  def paging = endpoint.get
    .in("paging")
    .in(query[Int]("page"))
    .out(htmlBodyUtf8)
    .handle { page =>
      either:
        connect(ds):
          val totalPages = repo
            .getPageCountMainQuery(pageSize)
            .left
            .map { t =>
              scribe.error("Failed to fetch page count", t)
            }
            .ok()

          val projects = repo
            .fetchNRecentlyReleasedProjectsWithArtifacts(pageSize, page)
            .left
            .map { t =>
              scribe.error("Failed to fetch projects", t)
            }
            .ok()

          val projectsAndArtifacts = Templates.ProjectAndArtifacts.fromDb(projects)

          val pagingNav = Templates.oobPagingNavigation(page, totalPages).render
          val table     = Templates.projectsTable(projectsAndArtifacts).render

          // out-of-bound swap of navigation + table
          s"""
             |$pagingNav
             |$table
             |""".stripMargin
    }

  def serveForever: Unit =
    scribe.info(s"Starting server at port ${config.port}...")
    NettySyncServer()
      .host("0.0.0.0")
      .port(config.port)
      .addEndpoint(staticResourcesGetServerEndpoint("static")(classOf[App].getClassLoader(), "static/"))
      .addEndpoint(paging)
      .addEndpoint(index)
      .startAndWait()
