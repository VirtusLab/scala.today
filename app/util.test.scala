package scala.today

import ox.*
import ox.either.*
import scala.util.control.*

class ImprovedEitherBlockTest extends munit.FunSuite:

  case class ComparableEx(msg: String) extends Exception(msg)

  test(".? works in usual case") {
    def failingMethod: Either[Throwable, Unit] = Left(ComparableEx("error"))

    val result = either:
      failingMethod.?
      "success"

    assertEquals(result, Left(ComparableEx("error")))
  }

  test(".? combinator prevents multiple enclosing either calls with different error types".ignore) {
    def inner(f: => Either[Unit, String]): Unit =
      try
        f
        fail("unexpected success")
      catch case NonFatal(e) => fail(e.getClass().getName())

    def failingMethod: Either[Throwable, Unit] = Left(ComparableEx("error"))

    def outer: Either[Throwable, Unit] = either:
      inner { // this runs on a separate thread or captures exceptions
        either:
          failingMethod.?
          "success"
      }

    assertEquals(outer, Left(ComparableEx("inner error")))
  }
