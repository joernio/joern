import scala.sys.process.stringToProcess
import scala.util.Try
import versionsort.VersionHelper
import com.typesafe.config.{Config, ConfigFactory}

name := "jssrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val astGenVersion = settingKey[String]("astgen version")
astGenVersion := appProperties.value.getString("jssrc2cpg.astgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := false

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWinAmd64   = "astgen-win.exe"
lazy val AstgenLinuxAmd64 = "astgen-linux"
lazy val AstgenLinuxArmV8 = "astgen-linux-arm"
lazy val AstgenMacAmd64   = "astgen-macos"
lazy val AstgenMacArmV8   = "astgen-macos-arm"

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
    Seq(AstgenWinAmd64, AstgenLinuxAmd64, AstgenLinuxArmV8, AstgenMacAmd64, AstgenMacArmV8)
  } else {
    (Environment.operatingSystem, Environment.architecture) match {
      case (Environment.OperatingSystemType.Windows, _)                                => Seq(AstgenWinAmd64)
      case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)   => Seq(AstgenLinuxAmd64)
      case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8) => Seq(AstgenLinuxArmV8)
      case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)     => Seq(AstgenMacAmd64)
      case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)   => Seq(AstgenMacArmV8)
      case _ => Seq(AstgenWinAmd64, AstgenLinuxAmd64, AstgenLinuxArmV8, AstgenMacAmd64, AstgenMacArmV8)
    }
  }
}

lazy val astGenDlTask = taskKey[Unit](s"Download astgen binaries")
astGenDlTask := {
  val astGenDir = baseDirectory.value / "bin" / "astgen"

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
  Package.ManifestAttributes(new java.util.jar.Attributes.Name("JS-AstGen-Version") -> astGenVersion.value)
