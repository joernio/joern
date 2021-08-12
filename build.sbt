import net.lingala.zip4j.model.ZipParameters

enablePlugins(GitVersioning)

name := "joern"
organization := "io.shiftleft"
ThisBuild / scalaVersion := "2.13.5"
// don't upgrade to 2.13.6 until https://github.com/com-lihaoyi/Ammonite/issues/1182 is resolved
ThisBuild /Test /fork := true
val cpgVersion = "1.3.271"
val ghidra2cpgVersion = "0.0.21"

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
  val joernCliZip = (joerncli/Universal/packageBin).value
  val ghidraStaged = (ghidra2cpg/Universal/stage).value

  val distributionZip = file("./joern-cli.zip")
  IO.copyFile(joernCliZip, distributionZip,
    CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true))

  // add ghidra2cpg
  import net.lingala.zip4j._
  val params = new ZipParameters()
  params.setRootFolderNameInZip("ghidra2cpg")
  params.setIncludeRootFolder(false)
  new ZipFile(distributionZip).addFolder(ghidraStaged, params)

  println(s"created distribution - resulting files: $distributionZip")
  distributionZip
}

Global / onChangedBuildSource := ReloadOnSourceChanges
