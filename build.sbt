name := "joern"
ThisBuild / organization := "io.joern"
ThisBuild / scalaVersion := "2.13.7"

val cpgVersion = "1.3.487"
val js2cpgVersion = "0.2.99"

lazy val joerncli = Projects.joerncli
lazy val querydb = Projects.querydb
lazy val console = Projects.console
lazy val dataflowengineoss = Projects.dataflowengineoss
lazy val macros = Projects.macros
lazy val fuzzyc2cpg = Projects.fuzzyc2cpg
lazy val c2cpg = Projects.c2cpg
lazy val ghidra2cpg = Projects.ghidra2cpg

ThisBuild / compile / javacOptions ++= Seq(
  "-g", //debug symbols
  "-Xlint")

ThisBuild/scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
)

lazy val createDistribution = taskKey[File]("Create a complete Joern distribution")
createDistribution := {
  val distributionFile = file("target/joern-cli.zip")
  val zip = (joerncli/Universal/packageBin).value

  IO.copyFile(zip, distributionFile)
  val querydbDistribution = (querydb/createDistribution).value

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Atlassian" at "https://packages.atlassian.com/mvn/maven-atlassian-external"
)

ThisBuild /Test /fork := true

Global / onChangedBuildSource := ReloadOnSourceChanges

// publishing info for sonatype / maven central
ThisBuild / publishTo := sonatypePublishToBundle.value
sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/joernio/joern"), "scm:git@github.com:joernio/joern.git"))
ThisBuild / homepage := Some(url("https://joern.io/"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  /* sonatype requires this to be non-empty */
  Developer(
    "fabsx00",
    "Fabian Yamaguchi",
    "fabs@shiftleft.io",
    url("https://github.com/fabsx00")
  )
)

publish / skip := true // don't publish the root project
