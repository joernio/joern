import scala.sys.process.stringToProcess
import scala.util.Try
import versionsort.VersionHelper
import com.typesafe.config.{Config, ConfigFactory}

name               := "jssrc2cpg"
scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.3.0")

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val astGenVersion = settingKey[String]("astgen version")
astGenVersion := appProperties.value.getString("jssrc2cpg.astgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"               %% "upickle"           % "2.0.0",
  "com.fasterxml.jackson.core" % "jackson-databind"  % "2.15.1",
  "com.typesafe"               % "config"            % "1.4.2",
  "com.michaelpollmeier"       % "versionsort"       % "1.0.11",
  "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

scalacOptions ++= Seq() ++ (
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq()
    case _ =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8",         // Specify character encoding used by source files.
        "-explaintypes", // Explain type errors in more detail.
        "-feature",      // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros", // Allow macro definition (besides implementation and application)
        "-language:higherKinds",         // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
        // "-Xfatal-warnings",              // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
        "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
        "-Xlint:option-implicit",        // Option.apply used implicit view.
        "-Xlint:package-object-classes", // Class or object defined in package object.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
        "-Ywarn-dead-code",              // Warn when dead code is identified.
        "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
        "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
        "-Ywarn-numeric-widen",          // Warn when numerics are widened.
        "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals",          // Warn if a local definition is unused.
        "-Ywarn-unused:params",          // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates"         // Warn if a private member is unused.
      )
  }
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := false

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWin    = "astgen-win.exe"
lazy val AstgenLinux  = "astgen-linux"
lazy val AstgenMac    = "astgen-macos"
lazy val AstgenMacArm = "astgen-macos-arm"

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/astgen/releases/download/v${astGenVersion.value}/"

def hasCompatibleAstGenVersion(astGenVersion: String): Boolean = {
  Try("astgen --version".!!).toOption.map(_.strip()) match {
    case Some(installedVersion) if installedVersion != "unknown" =>
      VersionHelper.compare(installedVersion, astGenVersion) >= 0
    case _ => false
  }
}

lazy val astGenBinaryNames = taskKey[Seq[String]]("astgen binary names")
astGenBinaryNames := {
  if (hasCompatibleAstGenVersion(astGenVersion.value)) {
    Seq.empty
  } else if (sys.props.get("ALL_PLATFORMS").contains("TRUE")) {
    Seq(AstgenWin, AstgenLinux, AstgenMac, AstgenMacArm)
  } else {
    Environment.operatingSystem match {
      case Environment.OperatingSystemType.Windows =>
        Seq(AstgenWin)
      case Environment.OperatingSystemType.Linux =>
        Seq(AstgenLinux)
      case Environment.OperatingSystemType.Mac =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(AstgenMac)
          case Environment.ArchitectureType.ARM => Seq(AstgenMacArm)
        }
      case Environment.OperatingSystemType.Unknown =>
        Seq(AstgenWin, AstgenLinux, AstgenMac, AstgenMacArm)
    }
  }
}

lazy val astGenDlTask = taskKey[Unit](s"Download astgen binaries")
astGenDlTask := {
  val astGenDir = baseDirectory.value / "bin" / "astgen"
  astGenDir.mkdirs()

  astGenBinaryNames.value.foreach { fileName =>
    val dest = astGenDir / fileName
    if (!dest.exists) {
      val url            = s"${astGenDlUrl.value}$fileName"
      val downloadedFile = SimpleCache.downloadMaybe(url)
      IO.copyFile(downloadedFile, dest)
    }
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(astGenDir, distDir)

  // permissions are lost during the download; need to set them manually
  astGenDir.listFiles().foreach(_.setExecutable(true, false))
  distDir.listFiles().foreach(_.setExecutable(true, false))
}

Compile / compile := ((Compile / compile) dependsOn astGenDlTask).value

lazy val astGenSetAllPlatforms = taskKey[Unit](s"Set ALL_PLATFORMS")
astGenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(astGenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

// Also remove astgen binaries with clean, e.g., to allow for updating them.
// Sadly, we can't define the bin/ folders globally,
// as .value can only be used within a task or setting macro
cleanFiles ++= Seq(
  baseDirectory.value / "bin" / "astgen",
  (Universal / stagingDirectory).value / "bin" / "astgen"
) ++ astGenBinaryNames.value.map(fileName => SimpleCache.encodeFile(s"${astGenDlUrl.value}$fileName"))

Universal / packageName       := name.value
Universal / topLevelDirectory := None
