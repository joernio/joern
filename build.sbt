enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild /Test /fork := true
val cpgVersion = "1.3.110"

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.mavenCentral,
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

lazy val createDistribution = taskKey[Unit]("Create a complete Joern distribution")
createDistribution := {
  (joerncli/Universal/packageZipTarball).value
  val joernCliZip = (joerncli/Universal/packageBin).value

  val cliZip = "./joern-cli.zip"
  IO.copy(
    List((joernCliZip, file(cliZip))),
    CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true)
  )
  println(s"created distribution - resulting files: $cliZip")
}

Global / onChangedBuildSource := ReloadOnSourceChanges
