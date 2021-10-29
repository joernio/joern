name := "joern"
ThisBuild / organization := "io.joern"
ThisBuild / scalaVersion := "2.13.5"
// don't upgrade to 2.13.6 until https://github.com/com-lihaoyi/Ammonite/issues/1182 is resolved

val cpgVersion = "1.3.404"
val js2cpgVersion = "0.2.24"
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

lazy val joerncli = Projects.joerncli
lazy val querydb = Projects.querydb
lazy val console = Projects.console
lazy val ghidra2cpg = Projects.ghidra2cpg

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
