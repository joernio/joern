ThisBuild / organization := "io.joern"
ThisBuild / scalaVersion := "3.8.3"
// sbt-native-packager defines ~190 Debian/RPM packaging keys that are unused in a standard build.
Global / lintUnusedKeysOnLoad := false

val cpgVersion = "1.7.76"

lazy val joerncli          = Projects.joerncli
lazy val querydb           = Projects.querydb
lazy val console           = Projects.console
lazy val dataflowengineoss = Projects.dataflowengineoss
lazy val macros            = Projects.macros
lazy val semanticcpg       = Projects.semanticcpg
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
lazy val abap2cpg          = Projects.abap2cpg
lazy val rust2cpg          = Projects.rust2cpg
lazy val linterRules       = Projects.linterRules
lazy val linterRulesInput  = Projects.linterRulesInput

lazy val createDistribution = taskKey[File]("Create a complete Joern distribution")

lazy val root = project
  .in(file("."))
  .aggregate(
    joerncli,
    querydb,
    console,
    dataflowengineoss,
    macros,
    semanticcpg,
    c2cpg,
    ghidra2cpg,
    x2cpg,
    pysrc2cpg,
    php2cpg,
    jssrc2cpg,
    javasrc2cpg,
    jimple2cpg,
    kotlin2cpg,
    rubysrc2cpg,
    gosrc2cpg,
    swiftsrc2cpg,
    csharpsrc2cpg,
    abap2cpg,
    rust2cpg,
    linterRules
    // linterRulesInput is intentionally NOT aggregated: it holds scalafix-testkit fixtures only
    // and must never be released (ciReleaseSonatype publishes root + aggregated projects)
  )
  .dependsOn(linterRules % ScalafixConfig)
  .settings(
    name         := "joern",
    publish / skip := true, // don't publish the root project
    createDistribution := Def.uncached {
      val platformSuffix = (Environment.operatingSystem, Environment.architecture) match {
        case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)     => "linux-x86_64"
        case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8)   => "linux-arm64"
        case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)       => "macos-x86_64"
        case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)     => "macos-arm64"
        case (Environment.OperatingSystemType.Windows, Environment.ArchitectureType.ARMv8) => "windows-arm64"
        case (Environment.OperatingSystemType.Windows, _)                                  => "windows-x86_64"
        case _                                                                              => "unknown"
      }
      val distributionFile    = target.value / s"joern-cli-$platformSuffix.zip"
      val zipRef              = (joerncli / Universal / packageBin).value
      val zip                 = fileConverter.value.toPath(zipRef).toFile
      IO.copyFile(zip, distributionFile)
      val querydbDistribution = (querydb / createDistribution).value
      println(s"created distribution - resulting files: $distributionFile, $querydbDistribution")
      distributionFile
    }
  )

ThisBuild / libraryDependencies ++= Seq(
  "org.slf4j"                % "slf4j-api"         % Versions.slf4j,
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % Versions.log4j % Optional,
  "org.apache.logging.log4j" % "log4j-core"        % Versions.log4j % Optional
  // `Optional` means "not transitive", but still included in "stage/lib"
)

// for heros 1.2.4 bug https://github.com/soot-oss/heros/commit/31f57b96a543de09092e7f351ed71115c80059ef
ThisBuild / dependencyOverrides ++= Seq(
  "com.google.guava" % "guava" % "33.5.0-jre"
)

ThisBuild / compile / javacOptions ++= Seq(
  "-g", // debug symbols
  "-Xlint",
  "-proc:none",
  "--release=17"
) ++ {
  // Require Java 13+ due to FileSystems.newFileSystem(Path) API used in project/FileUtils.scala
  val javaVersion = sys.props("java.specification.version").toFloat
  assert(javaVersion.toInt >= 13, s"this build requires JDK13+ - you're using $javaVersion")
  Nil
}

ThisBuild / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "--release",
  "17",
  "-Werror",
  "-feature",
  "-Wshadow:type-parameter-shadow",
  "-no-indent",
  "-old-syntax",
  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
)

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / resolvers ++= Seq(
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Atlassian" at "https://packages.atlassian.com/mvn/maven-atlassian-external",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases/",
  "Google Maven" at "https://maven.google.com/"
)

ThisBuild / Test / fork               := true
ThisBuild / Test / testForkedParallel := true

// sbt 2.x defaults to exportJars := true, putting internal subprojects on each other's classpaths as
// (potentially CAS-cached, extension-less) jars. That breaks tests which load resources as files
// (Paths.get(getResource(...).toURI) -> FileSystemNotFoundException) and in-process script compilation
// via replpp, whose inherited classpath entries the Scala compiler ignores when they have no .jar
// extension. Class directories avoid both issues.
ThisBuild / exportJars := false

trapExit := false

Global / onChangedBuildSource := ReloadOnSourceChanges

// publishing info for maven central (SBT 2.x built-in Central Portal publishing)
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

ThisBuild / scmInfo  := Some(ScmInfo(uri("https://github.com/joernio/joern"), "scm:git@github.com:joernio/joern.git"))
ThisBuild / homepage := Some(uri("https://joern.io/"))
ThisBuild / licenses := List(License("Apache-2.0", uri("http://www.apache.org/licenses/LICENSE-2.0")))
ThisBuild / developers := List(
  /* sonatype requires this to be non-empty */
  Developer("fabsx00", "Fabian Yamaguchi", "fabs@shiftleft.io", uri("https://github.com/fabsx00"))
)

ThisBuild / Test / packageBin / publishArtifact := true

// trigger an sbt reload when any `application.conf` file changes
Global / checkBuildSources / fileInputs += (baseDirectory.value.toGlob / ** / "resources" / "application.conf")
