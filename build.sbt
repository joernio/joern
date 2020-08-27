enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
/* n.b. skip 2.13.1, it has a regression https://github.com/scala/bug/issues/11754,
 * which is fixed in https://github.com/scala/scala/pull/8447, i.e. we can upgrade
 * to 2.13.2 once that's released */
ThisBuild / scalaVersion := "2.13.0"
ThisBuild /Test /fork := true

val cpgVersion = "0.11.400"
val fuzzyc2cpgVersion = "1.1.73"

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
lazy val schemaExtender = Projects.schemaExtender

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
