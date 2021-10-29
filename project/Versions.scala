/* reads version declarations from /build.sbt so that we can declare them in one place */
object Versions {
  val cpg = parseVersion("cpgVersion")
  val js2cpg = parseVersion("js2cpgVersion")
  val jimple2cpg = parseVersion("jimple2cpgVersion")
  val scalatest = "3.1.1"

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
      s"""unable to extract $key from build.sbt, expected exactly one line like `val $key= "0.0.0-SNAPSHOT"`.""")
    versions.head
  }

}

