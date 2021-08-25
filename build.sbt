enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
ThisBuild / scalaVersion := "2.13.5"
// don't upgrade to 2.13.6 until https://github.com/com-lihaoyi/Ammonite/issues/1182 is resolved
ThisBuild /Test /fork := true
val cpgVersion = "1.3.302"
val ghidra2cpgVersion = "0.0.25"
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
createDistribution := {
  import net.lingala.zip4j._

  val distributionFile = file("target/joern-cli.zip")
  val distributionZip = new ZipFile(distributionFile)

  distributionZip.addFile((joerncli/Universal/packageBin).value, withName("joern-cli.zip"))
  distributionZip.addFile((ghidra2cpg/Universal/packageBin).value, withName("ghidra2cpg.zip"))
  distributionZip.addFile(
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/js2cpg/releases/download/v$js2cpgVersion/js2cpg.zip"),
    withName("js2cpg.zip"))
  distributionZip.addFile(
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v$cpgVersion/fuzzy2cpg.zip"),
    withName("fuzzyc2cpg.zip"))
  distributionZip.addFile(
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v$cpgVersion/c2cpg.zip"),
    withName("c2cpg.zip"))

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

import net.lingala.zip4j.model.ZipParameters
def withName(name: String): ZipParameters = {
  val zipParams = new ZipParameters
  zipParams.setFileNameInZip(name)
  zipParams
}

Global / onChangedBuildSource := ReloadOnSourceChanges
