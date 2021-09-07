enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
ThisBuild / scalaVersion := "2.13.5"
// don't upgrade to 2.13.6 until https://github.com/com-lihaoyi/Ammonite/issues/1182 is resolved
ThisBuild /Test /fork := true
val cpgVersion = "1.3.313"
val c2cpgVersion = "1.3.313"
val fuzzyc2cpgVersion = "1.3.313"
val ghidra2cpgVersion = "0.0.27"
val js2cpgVersion = "0.2.3"

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.jcenterRepo,
  "jitpack" at "https://jitpack.io",
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"
)

ThisBuild/scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
)

scmInfo := Some(
  ScmInfo(url("https://github.com/ShiftLeftSecurity/joern"), "scm:git@github.com:ShiftLeftSecurity/joern.git"))
homepage := Some(url("https://joern.io/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val joerncli = Projects.joerncli
lazy val ghidra2cpg = Projects.ghidra2cpg

lazy val createDistribution = taskKey[File]("Create a complete Joern distribution")
import net.lingala.zip4j._
import net.lingala.zip4j.model.ZipParameters
createDistribution := {
  val distributionFile = file("target/joern-cli.zip")
  val distributionZip = new ZipFile(distributionFile)

  distributionZip.addFile((joerncli/Universal/packageBin).value, withName("joern-cli.zip"))
  distributionZip.addFile(Frontends.downloadC2CpgZip, withName("c2cpg.zip"))
  distributionZip.addFile(Frontends.downloadFuzzyc2CpgZip, withName("fuzzyc2cpg.zip"))
  distributionZip.addFile(Frontends.downloadJs2CpgZip, withName("js2cpg.zip"))

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

def withName(name: String): ZipParameters = {
  val zipParams = new ZipParameters
  zipParams.setFileNameInZip(name)
  zipParams
}



Global / onChangedBuildSource := ReloadOnSourceChanges
