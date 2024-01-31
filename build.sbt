name                     := "joern"
ThisBuild / organization := "io.joern"
ThisBuild / scalaVersion := "3.3.1"

val cpgVersion = "1.4.34"

lazy val joerncli          = Projects.joerncli
lazy val querydb           = Projects.querydb
lazy val console           = Projects.console
lazy val dataflowengineoss = Projects.dataflowengineoss
lazy val macros            = Projects.macros
lazy val semanticcpg       = Projects.semanticcpg
lazy val benchmarks        = Projects.benchmarks
lazy val c2cpg             = Projects.c2cpg
lazy val ghidra2cpg        = Projects.ghidra2cpg
lazy val x2cpg             = Projects.x2cpg
lazy val pysrc2cpg         = Projects.pysrc2cpg
lazy val php2cpg           = Projects.php2cpg
lazy val jssrc2cpg         = Projects.jssrc2cpg
lazy val javasrc2cpg       = Projects.javasrc2cpg
lazy val jimple2cpg        = Projects.jimple2cpg
lazy val kotlin2cpg        = Projects.kotlin2cpg
lazy val rubysrc2cpg       = Projects.rubysrc2cpg
lazy val gosrc2cpg         = Projects.gosrc2cpg
lazy val swiftsrc2cpg      = Projects.swiftsrc2cpg
lazy val csharpsrc2cpg     = Projects.csharpsrc2cpg

lazy val aggregatedProjects: Seq[ProjectReference] = Seq(
  joerncli,
  querydb,
  console,
  dataflowengineoss,
  macros,
  semanticcpg,
  c2cpg,
  x2cpg,
  pysrc2cpg,
  php2cpg,
  ghidra2cpg,
  jssrc2cpg,
  javasrc2cpg,
  jimple2cpg,
  kotlin2cpg,
  rubysrc2cpg,
  gosrc2cpg,
  swiftsrc2cpg,
  csharpsrc2cpg
)

ThisBuild / libraryDependencies ++= Seq(
  "org.slf4j"                % "slf4j-api"         % "2.0.7",
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.20.0" % Optional,
  "org.apache.logging.log4j" % "log4j-core"        % "2.20.0" % Optional
  // `Optional` means "not transitive", but still included in "stage/lib"
)

ThisBuild / compile / javacOptions ++= Seq(
  "-g", // debug symbols
  "-Xlint",
  "--release=11"
) ++ {
  // fail early if users with JDK8 try to run this
  val javaVersion = sys.props("java.specification.version").toFloat
  assert(javaVersion.toInt >= 11, s"this build requires JDK11+ - you're using $javaVersion")
  Nil
}

ThisBuild / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "--release",
  "11"
)

lazy val createDistribution = taskKey[File]("Create a complete Joern distribution")
createDistribution := {
  val distributionFile = file("target/joern-cli.zip")
  val zip              = (joerncli / Universal / packageBin).value

  IO.copyFile(zip, distributionFile)
  val querydbDistribution = (querydb / createDistribution).value

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Atlassian" at "https://packages.atlassian.com/mvn/maven-atlassian-external",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases/"
)

ThisBuild / Test / fork := true

Global / onChangedBuildSource := ReloadOnSourceChanges

// publishing info for sonatype / maven central
ThisBuild / publishTo  := sonatypePublishToBundle.value
sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / scmInfo    := Some(ScmInfo(url("https://github.com/joernio/joern"), "scm:git@github.com:joernio/joern.git"))
ThisBuild / homepage   := Some(url("https://joern.io/"))
ThisBuild / licenses   := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  /* sonatype requires this to be non-empty */
  Developer("fabsx00", "Fabian Yamaguchi", "fabs@shiftleft.io", url("https://github.com/fabsx00"))
)

publish / skip := true // don't publish the root project

// Avoids running root tasks on the benchmarks project
lazy val root = project
  .in(file("."))
  .aggregate(aggregatedProjects*)

ThisBuild / Test / packageBin / publishArtifact := true
