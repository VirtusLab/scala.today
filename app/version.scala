package scala.today

import scala.util.Try
import ox.catching
import com.google.protobuf.TextFormat.ParseException

/** A semantic version as defined by https://semver.org/
  *
  * @param major
  *   Major version number
  * @param minor
  *   Minor version number
  * @param patch
  *   Patch version number
  * @param epoch
  *   Optional Epoch version identifier
  * @param preRelease
  *   Pre-release version identifier
  * @param buildMetadata
  *   Build metadata version identifier
  */
case class Version(
  mainVersionNumbers: Vector[Long],
  preRelease: Option[String] = None,
  buildMetadata: Option[String] = None,
  original: String
) extends Ordered[Version]:
  override def compare(that: Version): Int =
    import math.Ordered.orderingToOrdered

    val mainCompared = mainVersionNumbers
      .zipAll(that.mainVersionNumbers, 0L, 0L)
      .map((thisNum, thatNum) => thisNum.compare(thatNum))
      .find(_ != 0)
      .getOrElse(0)

    // for pre release compare each dot separated identifier from left to right
    lazy val thisPreRelease: Vector[Option[String]] = preRelease.map(_.split('.')).getOrElse(Array.empty[String]).toVector.map(Some(_))
    lazy val thatPreRelease: Vector[Option[String]] = that.preRelease.map(_.split('.')).getOrElse(Array.empty[String]).toVector.map(Some(_))

    def comparePreReleaseIdentifier(thisIdentifier: Option[String], thatIdentifier: Option[String]): Int =
      (thisIdentifier, thatIdentifier) match
        case (Some(thisId), Some(thatId)) =>
          val thisIsNumeric = thisId.forall(_.isDigit) && thisId.length > 0
          val thatIsNumeric = thatId.forall(_.isDigit) && thatId.length > 0
          (thisIsNumeric, thatIsNumeric) match
            case (true, true)   => thisId.toLong.compare(thatId.toLong)
            case (false, false) => thisId.compare(thatId)
            case (true, false)  => -1 // numeric identifiers have always lower precedence than non-numeric identifiers
            case (false, true)  => 1 // numeric identifiers have always lower precedence than non-numeric identifiers
        /* A larger set of pre-release fields has a higher precedence than a smaller set, if all of the preceding identifiers are equal. */
        case (Some(_), None) => 1 // larger set of pre-release fields has higher precedence
        case (None, Some(_)) => -1 // larger set of pre-release fields has higher precedence
        case (None, None)    => 0

    lazy val preCompared: Int =
      thisPreRelease
        .zipAll(thatPreRelease, None, None)
        .map(comparePreReleaseIdentifier.tupled)
        .find(_ != 0)
        .getOrElse(
          thisPreRelease.length.compare(thatPreRelease.length)
        ) // if all identifiers are equal, the version with fewer fields has lower precedence

    // ignore build metadata when comparing versions per semver spec https://semver.org/#spec-item-10

    if mainCompared != 0
    then mainCompared
    else if thisPreRelease.isEmpty && thatPreRelease.nonEmpty
    then 1 // normal version has higher precedence than a pre-release version
    else if thisPreRelease.nonEmpty && thatPreRelease.isEmpty
    then -1 // normal version has higher precedence than a pre-release version
    else preCompared // pre-release version has lower precedence than a normal version
  end compare

  def isSnapshot: Boolean = preRelease.contains("SNAPSHOT")

  lazy val preReleaseString: String    = preRelease.map("-" + _).getOrElse("")
  lazy val buildMetadataString: String = buildMetadata.map("+" + _).getOrElse("")

  override def toString: String = s"${mainVersionNumbers.mkString(".")}$preReleaseString$buildMetadataString"
end Version

object Version:
  case class ParseException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

  import fastparse.{parse => fpParse, *}, NoWhitespace.*

  object parser:
    // TODO test for negative numbers
    def number[$: P]: P[Long] = P(CharIn("0-9").rep(min = 1, max = 19).!.map(_.toLong))

    def mainVersionNumbers[$: P] = number.rep(min = 1, sep = ".")

    def parseVersion[$: P](original: String): P[Version] =
      P(mainVersionNumbers ~ ("-" ~ CharsWhile(_ != '+').!).? ~ ("+" ~ CharsWhile(_ != ' ').!).?).map {
        case (mainVersionNums, preRelease, buildMetadata) =>
          Version(mainVersionNums.toVector, preRelease, buildMetadata, original)
      }

    def parse(s: String): Parsed[Version] = fpParse(s, parseVersion(s)(_))

  end parser

  def parse(version: String, original: String): Either[Exception, Version] = Try {
    fpParse(version, parser.parseVersion(original)(_)) match
      case Parsed.Success(version, _) => version
      case Parsed.Failure(expected, failIndex, extra) =>
        throw ParseException(
          s"Cannot parse as semantic version: '$original' due to:\n" +
            s" - expected: $expected\n" + s" - failIndex: $failIndex\n"
        )
  }.toEither.left.map(e => ParseException(s"Cannot parse as semantic version: '$original'", e))

  /** ParseTolerant allows for certain version specifications that do not strictly adhere to semver specs to be parsed by this library. It does so by
    * normalizing versions before passing them to [[parse]]. It currently trims spaces, removes a "v" prefix, and adds a 0 patch number to versions
    * with only major and minor components specified.
    */
  def parseTolerant(version: String): Either[Exception, Version] = catching {
    def handleMajorMinorWithPrerelease(parts: Array[String], char: Char): String =
      val index        = parts.last.indexOf(char)
      val minorVersion = parts.last.split(char).head
      val baseVersion  = (parts.dropRight(1) ++ Array(minorVersion, "0")).mkString(".")
      baseVersion + parts.last.drop(index)

    val str = version.trim.stripPrefix("v")

    // Split into major.minor.(patch+pr+meta)
    val parts = str.split("\\.", 3)
    if parts.length < 3 then
      if parts.last.contains("+") then
        val baseVersionWithSuffix = handleMajorMinorWithPrerelease(parts, '+')
        parse(baseVersionWithSuffix, version)
      else if parts.last.contains("-") then
        val baseVersionWithSuffix = handleMajorMinorWithPrerelease(parts, '-')
        parse(baseVersionWithSuffix, version)
      else parse((parts.toList ::: List.fill(3 - parts.length)("0")).mkString("."), version)
    else parse(str, version)
  }.flatten.left.map {
    case pe: ParseException => pe
    case e                  => ParseException(s"Cannot parse as semantic version: '$version'", e)
  }
