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

  astGenBinaryNames.value.foreach { fileName =>
    DownloadHelper.ensureIsAvailable(s"${astGenDlUrl.value}$fileName", astGenDir / fileName)
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

Universal / packageName       := name.value
Universal / topLevelDirectory := None

githubOwner      := "Privado-Inc"
githubRepository := "joern"

credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
