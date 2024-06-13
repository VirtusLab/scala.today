package scala.today

import scala.util.Try
import ox.*
import scala.concurrent.duration.*
import com.augustnagro.magnum.Frag

extension [A](expr: => A) def attempt: Either[Throwable, A] = Try(expr).toEither

extension [A](expr: => A)
  inline def as[B](b: B): B = b

  def sleepAfter(duration: Duration)(using Ox): A =
    val result = expr
    Thread.sleep(duration.toMillis)
    result

extension [E <: Throwable, A](either: Either[E, A])
  def discardError: Either[Unit, A] =
    either.left.map(_ => ())
  def logErrorDiscard(msg: String): Either[Unit, A] =
    either.left.map(e => scribe.error(msg, e))
  def logError(msg: String): Either[E, A] =
    either.left.map(e => scribe.error(msg, e).as(e))

extension (frag: Frag)
  def logQuery: Frag =
    scribe.info(frag.sqlString)
    frag

trait OxApp:
  def main(args: Array[String]): Unit =
    supervised:
      val forkedMain = fork(supervised(run(args.toVector)))
      forkedMain.joinEither() match
        case Left(err) => throw err
        case Right(userEither) =>
          userEither match
            case Left(err) => throw err
            case Right(()) => System.exit(0)

  def run(args: Vector[String])(using Ox): Either[Throwable, Unit]

extension (t: Throwable)
  def printToString: String =
    val sw = new java.io.StringWriter
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)
    sw.toString
