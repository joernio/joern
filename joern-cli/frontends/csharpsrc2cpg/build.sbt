import com.typesafe.config.{Config, ConfigFactory}
import versionsort.VersionHelper

import scala.sys.process.stringToProcess
import scala.util.Try

name := "csharpsrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val astGenVersion = settingKey[String]("dotnetastgen version")
astGenVersion := appProperties.value.getString("csharpsrc2cpg.dotnetastgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := false

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWin      = "dotnetastgen-win.exe"
lazy val AstgenLinux    = "dotnetastgen-linux"
lazy val AstgenLinuxArm = "dotnetastgen-linux-arm64"
lazy val AstgenMac      = "dotnetastgen-macos"

lazy val AllPlatforms = Seq(AstgenWin, AstgenLinux, AstgenLinuxArm, AstgenMac)

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
      case Environment.OperatingSystemType.Windows => Seq(AstgenWin)
      case Environment.OperatingSystemType.Linux =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86   => Seq(AstgenLinux)
          case Environment.ArchitectureType.ARMv8 => Seq(AstgenLinuxArm)
        }
      case Environment.OperatingSystemType.Mac => Seq(AstgenMac)
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
    val file = astGenDir / fileName
    DownloadHelper.ensureIsAvailable(s"${astGenDlUrl.value}$fileName", file)
    // permissions are lost during the download; need to set them manually
    file.setExecutable(true, false)
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(astGenDir, distDir, preserveExecutable = true)
}

Compile / compile := ((Compile / compile) dependsOn astGenDlTask).value

lazy val astGenSetAllPlatforms = taskKey[Unit](s"Set ALL_PLATFORMS")
astGenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(astGenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes(new java.util.jar.Attributes.Name("DotNet-AstGen-Version") -> astGenVersion.value)
