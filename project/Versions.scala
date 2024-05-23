/* reads version declarations from /build.sbt so that we can declare them in one place */
object Versions {
  val cpg = parseVersion("cpgVersion")
  // Dont upgrade antlr to 4.10 or above since those versions require java 11 or higher which
  // causes problems upstreams.
  val antlr                  = "4.7.2"
  val cask                   = "0.9.2"
  val catsCore               = "2.10.0"
  val catsEffect             = "3.5.4"
  val cfr                    = "0.152"
  val commonsCompress        = "1.26.1"
  val commonsIo              = "2.16.0"
  val commonsLang            = "3.14.0"
  val commonsText            = "1.12.0"
  val eclipseCdt             = "8.4.0.202401242025"
  val eclipseCore            = "3.20.100"
  val eclipseText            = "3.14.0"
  val ghidra                 = "11.0_PUBLIC_20231222-2"
  val gradleTooling          = "8.3"
  val jacksonDatabind        = "2.17.0"
  val javaParser             = "3.25.9"
  val json4s                 = "4.0.7"
  val lombok                 = "1.18.32"
  val mavenArcheologist      = "0.0.10"
  val pPrint                 = "0.8.1"
  val reflection             = "0.10.2"
  val requests               = "0.8.0"
  val scalaParallel          = "1.0.4"
  val scalaParserCombinators = "2.4.0"
  val scalaReplPP            = "0.1.87"
  val scalatest              = "3.2.18"
  val scopt                  = "4.1.0"
  val semverParser           = "0.0.6"
  val soot                   = "4.5.0"
  val slf4j                  = "2.0.7"
  val log4j                  = "2.20.0"
  val upickle                = "3.3.0"
  val zeroTurnaround         = "1.17"

  // Shared with `projects/meta-build.sbt`, which needs to be updated there directly
  val betterFiles    = "3.9.2"
  val javaCc         = "7.0.12"
  val osLib          = "0.9.3"
  val typeSafeConfig = "1.4.3"
  val versionSort    = "1.0.11"
  val zip4j          = "2.11.5"

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
