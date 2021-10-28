name := "joern"
ThisBuild / organization := "io.joern"
ThisBuild / scalaVersion := "2.13.5"
// don't upgrade to 2.13.6 until https://github.com/com-lihaoyi/Ammonite/issues/1182 is resolved

val cpgVersion = "1.3.397"
val ghidra2cpgVersion = "0.0.51"
val js2cpgVersion = "0.2.21"
val javasrc2cpgVersion = "0.0.14"
val jimple2cpgVersion = "0.0.8"

ThisBuild /Test /fork := true

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.jcenterRepo,
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
lazy val querydb = Projects.querydb
lazy val console = Projects.console

lazy val createDistribution = taskKey[File]("Create a complete Joern distribution")
createDistribution := {
  val distributionFile = file("target/joern-cli.zip")
  val zip = (joerncli/Universal/packageBin).value

  IO.copyFile(zip, distributionFile)
  val querydbDistribution = (querydb/createDistribution).value

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

Global / onChangedBuildSource := ReloadOnSourceChanges
