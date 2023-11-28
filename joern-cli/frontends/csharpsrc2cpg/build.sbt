import better.files
import com.typesafe.config.{Config, ConfigFactory}
import versionsort.VersionHelper

import scala.sys.process.stringToProcess
import scala.util.Try

name := "csharpsrc2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val astGenVersion = settingKey[String]("dotnetastgen version")
astGenVersion := appProperties.value.getString("csharpsrc2cpg.dotnetastgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"               %% "upickle"           % Versions.upickle,
  "com.fasterxml.jackson.core" % "jackson-databind"  % "2.15.2",
  "com.typesafe"               % "config"            % "1.4.2",
  "com.michaelpollmeier"       % "versionsort"       % "1.0.11",
  "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := false

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWin      = "dotnetastgen-win.exe"
lazy val AstgenWinArm   = "dotnetastgen-win-arm.exe"
lazy val AstgenLinux    = "dotnetastgen-linux"
lazy val AstgenLinuxArm = "dotnetastgen-linux-arm"
lazy val AstgenMac      = "dotnetastgen-macos"
lazy val AstgenMacArm   = "dotnetastgen-macos-arm"

lazy val AllPlatforms = Seq(AstgenWin, AstgenWinArm, AstgenLinux, AstgenLinuxArm, AstgenMac, AstgenMacArm)

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/DotNetAstGen/releases/download/v${astGenVersion.value}/"

def hasCompatibleAstGenVersion(astGenVersion: String): Boolean = {
  Try("dotnetastgen --version".!!).toOption.map(_.strip()) match {
    case Some(installedVersion) if installedVersion != "unknown" =>
      VersionHelper.compare(installedVersion, astGenVersion) >= 0
    case _ => false
  }
}

lazy val astGenBinaryNames = taskKey[Seq[String]]("asstgen binary names")
astGenBinaryNames := {
  if (hasCompatibleAstGenVersion(astGenVersion.value)) {
    Seq.empty
  } else if (sys.props.get("ALL_PLATFORMS").contains("TRUE")) {
    AllPlatforms
  } else {
    Environment.operatingSystem match {
      case Environment.OperatingSystemType.Windows =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(AstgenWin)
          case Environment.ArchitectureType.ARM => Seq(AstgenWinArm)
        }
        Seq(AstgenWin)
      case Environment.OperatingSystemType.Linux =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(AstgenLinux)
          case Environment.ArchitectureType.ARM => Seq(AstgenLinuxArm)
        }
        Seq(AstgenLinux)
      case Environment.OperatingSystemType.Mac =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86 => Seq(AstgenMac)
          case Environment.ArchitectureType.ARM => Seq(AstgenMacArm)
        }
      case Environment.OperatingSystemType.Unknown =>
        AllPlatforms
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
      val url            = s"${astGenDlUrl.value}${fileName.stripSuffix(".exe")}.zip"
      val downloadedFile = files.File(SimpleCache.downloadMaybe(url).toPath)
      files.File.temporaryDirectory("joern-").apply { unzipTarget =>
        downloadedFile.unzipTo(unzipTarget)
        unzipTarget.list.filter(_.name == fileName).foreach(exec => IO.copyFile(exec.toJava, dest))
      }
      downloadedFile.delete(swallowIOExceptions = true)
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
