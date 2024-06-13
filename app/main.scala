package scala.today

import ox.*
import ox.either.*
import besom.cfg.*
import sttp.model.Uri
import besom.cfg.internal.ConfiguredType
import besom.cfg.internal.FieldType

given uriFromEnv(using FromEnv[String]): FromEnv[Uri] with
  def decode(env: Map[String, String], path: String): Option[Uri] =
    summon[FromEnv[String]].decode(env, path).flatMap(Uri.parse(_).toOption)

given ConfiguredType[Uri] with
  def toFieldType: FieldType = FieldType.String

case class Config(port: Int, jdbcUrl: String, dbUser: String, dbPassword: String, baseScaladexUrl: Uri, runScrapingJobs: Boolean) derives Configured

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
    if config.runScrapingJobs then
      fork:
        forever:
          runScrapingOncePerDay(config)

    // start the http server
    val http = Http(config, ds, repo, Search(repo, Http.pageSize))
    http.serveForever
