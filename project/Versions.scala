/* reads version declarations from /build.sbt so that we can declare them in one place */
object Versions {
  val cpg = parseVersion("cpgVersion")
  // Dont upgrade antlr to 4.10 or above since those versions require java 11 or higher which
  // causes problems upstreams.
  val antlr         = "4.7"
  val scalatest     = "3.2.15"
  val cats          = "3.3.14"
  val json4s        = "4.0.6"
  val gradleTooling = "7.5.1"

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
