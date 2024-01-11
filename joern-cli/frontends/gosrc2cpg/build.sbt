import scala.sys.process.stringToProcess
import scala.util.Try
import versionsort.VersionHelper
import com.typesafe.config.{Config, ConfigFactory}

name := "gosrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
  "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test,
  "com.lihaoyi"               %% "os-lib"            % "0.9.1",
  "com.fasterxml.jackson.core" % "jackson-databind"  % "2.15.2",
  "io.circe"                  %% "circe-core"        % Versions.circe,
  "io.circe"                  %% "circe-generic"     % Versions.circe,
  "io.circe"                  %% "circe-parser"      % Versions.circe
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val goAstGenVersion = settingKey[String]("goastgen version")
goAstGenVersion := appProperties.value.getString("gosrc2cpg.goastgen_version")

lazy val GoAstgenWin      = "goastgen-windows.exe"
lazy val GoAstgenLinux    = "goastgen-linux"
lazy val GoAstgenLinuxArm = "goastgen-linux-arm64"
lazy val GoAstgenMac      = "goastgen-macos"
lazy val GoAstgenMacArm   = "goastgen-macos-arm64"

lazy val goAstGenDlUrl = settingKey[String]("goastgen download url")
goAstGenDlUrl := s"https://github.com/Privado-Inc/goastgen/releases/download/v${goAstGenVersion.value}/"

def hasCompatibleAstGenVersion(goAstGenVersion: String): Boolean = {
  Try("goastgen -version".!!).toOption.map(_.strip()) match {
    case Some(installedVersion) if installedVersion != "unknown" =>
      VersionHelper.compare(installedVersion, goAstGenVersion) >= 0
    case _ => false
  }
}

lazy val goAstGenBinaryNames = taskKey[Seq[String]]("goastgen binary names")
goAstGenBinaryNames := {
  if (hasCompatibleAstGenVersion(goAstGenVersion.value)) {
    Seq.empty
  } else if (sys.props.get("ALL_PLATFORMS").contains("TRUE")) {
    Seq(GoAstgenWin, GoAstgenLinux, GoAstgenLinuxArm, GoAstgenMac, GoAstgenMacArm)
  } else {
    Environment.operatingSystem match {
      case Environment.OperatingSystemType.Windows =>
        Seq(GoAstgenWin)
      case Environment.OperatingSystemType.Linux =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(GoAstgenLinux)
          case Environment.ArchitectureType.ARM => Seq(GoAstgenLinuxArm)
        }
      case Environment.OperatingSystemType.Mac =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(GoAstgenMac)
          case Environment.ArchitectureType.ARM => Seq(GoAstgenMacArm)
        }
      case Environment.OperatingSystemType.Unknown =>
        Seq(GoAstgenWin, GoAstgenLinux, GoAstgenLinuxArm, GoAstgenMac, GoAstgenMacArm)
    }
  }
}

lazy val goAstGenDlTask = taskKey[Unit](s"Download goastgen binaries")
goAstGenDlTask := {
  val goAstGenDir = baseDirectory.value / "bin" / "astgen"
  goAstGenDir.mkdirs()

  goAstGenBinaryNames.value.foreach { fileName =>
    val dest = goAstGenDir / fileName
    if (!dest.exists) {
      val url            = s"${goAstGenDlUrl.value}$fileName"
      val downloadedFile = SimpleCache.downloadMaybe(url)
      IO.copyFile(downloadedFile, dest)
    }
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(goAstGenDir, distDir)

  // permissions are lost during the download; need to set them manually
  goAstGenDir.listFiles().foreach(_.setExecutable(true, false))
  distDir.listFiles().foreach(_.setExecutable(true, false))
}

Compile / compile := ((Compile / compile) dependsOn goAstGenDlTask).value

lazy val goAstGenSetAllPlatforms = taskKey[Unit](s"Set ALL_PLATFORMS")
goAstGenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(goAstGenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

// Also remove astgen binaries with clean, e.g., to allow for updating them.
// Sadly, we can't define the bin/ folders globally,
// as .value can only be used within a task or setting macro
cleanFiles ++= Seq(
  baseDirectory.value / "bin" / "astgen",
  (Universal / stagingDirectory).value / "bin" / "astgen"
) ++ goAstGenBinaryNames.value.map(fileName => SimpleCache.encodeFile(s"${goAstGenDlUrl.value}$fileName"))
