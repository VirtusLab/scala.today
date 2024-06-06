package scala.today

import ox.*
import ox.either.*
import besom.cfg.*

case class Config(port: Int, jdbcUrl: String, dbUser: String, dbPassword: String) derives Configured

object App extends OxApp:

  def run(args: Vector[String])(using Ox): Either[Throwable, Unit] = either:
    // resolve the configuration using besom-cfg
    val config = resolveConfiguration[Config]

    // create the database connections and repositories
    val repo = db.ProjectWithLatestReleaseRepo()
    val ds   = db.createDatasource(config.jdbcUrl, config.dbUser, config.dbPassword).ok()

    // run migrations before starting the server
    db.performMigrations(config.jdbcUrl, config.dbUser, config.dbPassword).ok()

    // fork the scraping job to run once per day
    fork:
      forever:
        runScrapingOncePerDay(config)

    // start the http server
    val http = Http(config, ds, repo)
    http.serveForever
