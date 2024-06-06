package scala.today

//noinspection ScalaFileName
class SemanticVersionTest extends munit.FunSuite:

  test("parse 1.0.0") {
    import fastparse.{parse => fpParse, *}, NoWhitespace.*
    import Version.parser

    assertEquals(fpParse("123", parser.number(_)), Parsed.Success(123L, 3))
    assertEquals(Version.parse("1.0.0", "1.0.0"), Right(Version(Vector(1, 0, 0), original = "1.0.0")))
  }

  test("parse a version with empty string in pre-release tag") {
    val version = "1.0.0-"

    assertEquals(Version.parseTolerant(version), Right(Version(Vector(1, 0, 0), None, None, original = version)))
  }

  test("fails to parse literal garbage") {
    val garbage = "B O R K E D1.2.3-SNAPSHOT"
    assertEquals(
      Version.parseTolerant(garbage),
      Left(
        Version.ParseException(
          s"Cannot parse as semantic version: 'B O R K E D1.2.3-SNAPSHOT'",
          Version.ParseException("""Cannot parse as semantic version: 'B O R K E D1.2.3-SNAPSHOT' due to:
 - expected: 
 - failIndex: 0
""")
        )
      )
    )
  }

  Map(
    "1.0.0" -> Version(Vector(1, 0, 0), original = "1.0.0"),
    "v1.0.0" -> Version(Vector(1, 0, 0), original = "v1.0.0"),
    "1.0.0-alpha" -> Version(Vector(1, 0, 0), Some("alpha"), original = "1.0.0-alpha"),
    "1.0.0-rc.1" -> Version(Vector(1, 0, 0), Some("rc.1"), original = "1.0.0-rc.1"),
    "0.0.1-SNAPSHOT" -> Version(Vector(0, 0, 1), Some("SNAPSHOT"), original = "0.0.1-SNAPSHOT"),
    "v0.0.1-SNAPSHOT" -> Version(Vector(0, 0, 1), Some("SNAPSHOT"), original = "v0.0.1-SNAPSHOT"),
    "1.0.0-alpha+001" -> Version(Vector(1, 0, 0), Some("alpha"), Some("001"), original = "1.0.0-alpha+001"),
    "1.0.0+20130313144700" -> Version(Vector(1, 0, 0), None, Some("20130313144700"), original = "1.0.0+20130313144700"),
    "1.0.0-beta+exp.sha.5114f85" -> Version(Vector(1, 0, 0), Some("beta"), Some("exp.sha.5114f85"), original = "1.0.0-beta+exp.sha.5114f85"),
    "1.0.0+21AF26D3----117B344092BD" -> Version(
      Vector(1, 0, 0),
      None,
      Some("21AF26D3----117B344092BD"),
      original = "1.0.0+21AF26D3----117B344092BD"
    ),
    "7.21.15.13" -> Version(Vector(7L, 21L, 15L, 13L), original = "7.21.15.13"),
    "1.0.0-" -> Version(Vector(1, 0, 0), None, None, original = "1.0.0-")
  ).foreachEntry((input, expected) =>
    test(s"parse $input") {
      assertEquals(Version.parseTolerant(input), Right(expected))
    }
  )

  Map(
    Version(Vector(0, 0, 1), Some("SNAPSHOT"), original = "0.0.1-SNAPSHOT") -> "0.0.1-SNAPSHOT"
  ).foreachEntry((input, expected) =>
    test(s"format $input") {
      assertEquals(input.toString, expected)
    }
  )

  Map(
    "0.0.0" -> "0.0.1",
    "0.0.1" -> "0.1.0",
    "0.1.0" -> "0.1.1",
    "1.0.0" -> "2.0.0",
    "1.0.0.0" -> "1.0.0.1",
    "1.0.0-alpha" -> "1.0.0",
    "1.0.0-alpha" -> "1.0.0-alpha.1",
    "1.0.0-alpha.1" -> "1.0.0-alpha.beta",
    "1.0.0-alpha.beta" -> "1.0.0-beta",
    "1.0.0-beta" -> "1.0.0-beta.2",
    "1.0.0-beta.2" -> "1.0.0-beta.11",
    "1.0.0-beta.11" -> "1.0.0-rc.1",
    "1.0.0-rc.1" -> "1.0.0",
    "1.0.0-alpha+001" -> "1.0.0-alpha.1",
    "1.0.0-alpha.1" -> "1.0.0-alpha.beta"
  ).foreachEntry((left, right) =>
    test(s"$left < $right") {
      assertEquals(Version.parse(left, left).toTry.get < Version.parse(right, right).toTry.get, true)
    }
  )

  test("1.0.0 == 1.0.0") {
    assertEquals(Version.parse("1.0.0", "1.0.0").toTry.get == Version.parse("1.0.0", "1.0.0").toTry.get, true)
  }
end SemanticVersionTest
