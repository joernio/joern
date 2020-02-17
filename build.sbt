enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
/* n.b. skip 2.13.1, it has a regression https://github.com/scala/bug/issues/11754,
 * which is fixed in https://github.com/scala/scala/pull/8447, i.e. we can upgrade
 * to 2.13.2 once that's released */
ThisBuild / scalaVersion := "2.13.0"

val cpgVersion = "0.11.69+0-790569b0+20200217-2016"
val fuzzyc2cpgVersion = "1.1.27"

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.bintrayRepo("shiftleft", "maven"),
  Resolver.bintrayRepo("mpollmeier", "maven"),
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
)

ThisBuild/scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
)

scmInfo := Some(
  ScmInfo(url("https://github.com/ShiftLeftSecurity/joern"), "scm:git@github.com:ShiftLeftSecurity/joern.git"))
homepage := Some(url("https://joern.io/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val joerncli = Projects.joerncli
lazy val joernserver = Projects.joernserver

lazy val createDistribution = taskKey[Unit]("Create a complete Joern distribution")
createDistribution := {
  val joernCliZip = (joerncli / Universal / packageBin).value
  val joernServerZip = (joernserver / Universal / packageBin).value

  IO.copy(
    List((joernCliZip, file("./joern-cli.zip")), (joernServerZip, file("./joern-server.zip"))),
    CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true)
  )
}

Global / onChangedBuildSource := ReloadOnSourceChanges
