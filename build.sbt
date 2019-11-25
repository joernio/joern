enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
ThisBuild / scalaVersion := "2.12.8"

val cpgVersion = "0.10.146"
val fuzzyc2cpgVersion = "1.1.16"

ThisBuild / resolvers += Resolver.mavenLocal

ThisBuild / resolvers += Resolver.bintrayRepo("shiftleft", "maven")

ThisBuild / resolvers += "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"

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
