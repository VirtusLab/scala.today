package scala.today

import scala.util.Try
import ox.*

extension [A](expr: => A) def attempt: Either[Throwable, A] = Try(expr).toEither

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

import scala.compiletime.{error, summonFrom}
import scala.util.boundary, boundary.Label, boundary.break

extension [E, A](inline t: Either[E, A])
  /** Unwrap the value of the `Either`, short-circuiting the computation to the enclosing [[either]], in case this is a left-value. */
  transparent inline def ? =
    summonFrom {
      case given boundary.Label[Either[E, Nothing]] =>
        // summonFrom {
        //   case given boundary.Label[Either[Nothing, Nothing]] if given_Label_Either != summon[boundary.Label[Either[E, Nothing]]] =>
        //     error(
        //       "There are multiple enclosing `either` calls with different error types.\nMake sure you're using .? inside of a single, separate either: block."
        //     )
        // }
        t match
          case Left(e)  => break(Left(e))
          case Right(a) => a
      case given boundary.Label[Either[Nothing, Nothing]] =>
        error("The enclosing `either` call uses a different error type.\nIf it's explicitly typed, is the error type correct?")
      case _ => error("`.ok()` can only be used within an `either` call.\nIs it present?")
    }: A
