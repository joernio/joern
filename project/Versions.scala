/* reads version declarations from /build.sbt so that we can declare them in one place */
object Versions {
  val cpg = parseVersion("cpgVersion")
  // Dont upgrade antlr to 4.10 or above since those versions require java 11 or higher which
  // causes problems upstreams.
  val antlr           = "4.7.2"
  val cask            = "0.9.2"
  val cats            = "3.5.4"
  val catsCore        = "2.10.0"
  val cfr             = "0.152"
  val commonsLang     = "3.14.0"
  val gradleTooling   = "8.3"
  val jacksonDatabind = "2.17.0"
  val json4s          = "4.0.7"
  val osLib           = "0.9.3"
  val pPrint          = "0.8.1"
  val reflection      = "0.10.2"
  val requests        = "0.8.0"
  val scalaReplPP     = "0.1.85"
  val scalatest       = "3.2.18"
  val scopt           = "4.1.0"
  val soot            = "4.4.1"
  val typeSafeConfig  = "1.4.3"
  val upickle         = "3.3.0"
  val versionSort     = "1.0.11"
  val zeroTurnaround  = "1.17"

  private def parseVersion(key: String): String = {
    val versionRegexp = s""".*val $key[ ]+=[ ]?"(.*?)"""".r
    val versions: List[String] = scala.io.Source
      .fromFile("build.sbt")
      .getLines
      .filter(_.contains(s"val $key"))
      .collect { case versionRegexp(version) => version }
      .toList
    assert(
      versions.size == 1,
      s"""unable to extract $key from build.sbt, expected exactly one line like `val $key= "0.0.0-SNAPSHOT"`."""
    )
    versions.head
  }

}
