package io.joern.x2cpg.utils

/** Parses the given string as if it contains a semver version, or refers to some regex matching some other semver
  * versions.
  *
  * @see
  *   <a href="https://semver.org">SemVer</a>
  */
object SemVer {

  /** Parses a raw semver string and coverts fuzzy semver to a regular semver with some heuristics.
    * @param rawString
    *   the string to parse.
    * @return
    *   a regular SemVer object.
    */
  def apply(rawString: String): SemVer = {

    def parse(regularString: String): SemVer = {
      val buildStart      = regularString.indexOf("+")
      val hasBuild        = buildStart > 0
      val preReleaseStart = regularString.indexOf("-")
      val hasPreRelease   = preReleaseStart > 0 && (if hasBuild then preReleaseStart < buildStart else true)

      val versionString =
        if (hasPreRelease) regularString.substring(0, preReleaseStart)
        else if (hasBuild) regularString.substring(0, buildStart)
        else regularString

      if versionString.endsWith(".") || versionString.startsWith(".") then
        throw SemVerParsingException(s"Unable to parse core version '$versionString'")

      val (major, minor, patch) = parseVersionCore(versionString)
      if (major < 0 || minor < 0 || patch < 0)
        throw new SemVerParsingException(s"Found a negative integer within the core version '$versionString'")

      val preRelease =
        if (hasPreRelease && hasBuild)
          regularString.substring(versionString.length, buildStart).split("-").filterNot(_.isBlank).toList
        else if (hasPreRelease) regularString.substring(versionString.length).split("-").filterNot(_.isBlank).toList
        else Nil

      val build =
        if (hasBuild) Option(regularString.substring(buildStart + 1))
        else None

      SemVer(major, minor, patch, preRelease, build)
    }

    if (rawString.isBlank) throw SemVerParsingException("Blank string given")

    val regularString = rawString
      .replaceAll("^(<=|>=|==|!=|~=|>|<)", "") // ignore equality prefixes
      .trim
    parse(regularString)
  }

  private def parseVersionCore(versionCore: String): (Int, Int, Int) = {
    versionCore
      .split("[.]")
      .map {
        case "*" => 0 // ignore fuzzy version
        case x   => Integer.parseInt(x)
      }
      .toList match {
      case major :: minor :: patch :: xs => (major, minor, patch)
      case major :: minor :: Nil         => (major, minor, 0)
      case major :: Nil                  => (major, 0, 0)
      case Nil => throw new SemVerParsingException("Empty core version found while parsing sem-ver version!")
    }
  }

}

/** Thrown whenever one cannot parse a semantic version string.
  *
  * @param msg
  *   the error details.
  * @see
  *   * <a href="https://semver.org">SemVer</a>
  */
class SemVerParsingException(msg: String) extends RuntimeException(msg)

/** A semantic version objects.
  * @param major
  *   the first integer in the core version.
  * @param minor
  *   the second integer in the core version.
  * @param patch
  *   the third integer in the core version.
  * @param preRelease
  *   an optional suffix after the core version delimited by hyphens.
  * @param build
  *   an optional suffix after pre-release (if specified) delimited by a plus.
  */
case class SemVer(
  major: Int = 0,
  minor: Int = 0,
  patch: Int = 0,
  preRelease: List[String] = List.empty,
  build: Option[String] = None
) extends Comparable[SemVer]
    with Ordered[SemVer] {

  /** Compares two SemVer objects. Note `build` is ignored during precedence checking.
    * @param t
    *   the other SemVer object.
    * @return
    *   > 0 if this object is greater than the other, 0 if both are equal, and < 0 if this object is less than the
    *   other.
    */
  override def compareTo(t: SemVer): Int = {
    if (this.major != t.major) {
      this.major.compareTo(t.major)
    } else if (this.minor != t.minor) {
      this.minor.compareTo(t.minor)
    } else if (this.patch != t.patch) {
      this.patch.compareTo(t.patch)
    } else {
      (this.preRelease, t.preRelease) match {
        case (Nil, Nil) => 0
        case (xs, Nil)  => -1
        case (Nil, ys)  => 1
        case (xs, ys)   =>
          // This not strictly correct and doesn't handle size differences "correctly" but this is probably good enough
          xs.lazyZip(ys).map { case (x, y) => x.compareTo(y) }.collectFirst { case x if x != 0 => x }.getOrElse(0)
      }
    }
  }

  override def compare(that: SemVer): Int = this.compareTo(that)

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: SemVer => this.compareTo(o) == 0
      case _         => false
    }
  }

  override def toString: String = {
    val sb = StringBuilder(s"$major.$minor.$patch")
    preRelease match {
      case Nil => // do nothing
      case xs  => sb.append("-").append(xs.mkString("-"))
    }
    build match {
      case Some(buildInfo) => sb.append("+").append(buildInfo)
      case None            => // do nothing
    }
    sb.toString
  }

}
