import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.sbt.packager.Keys.stagingDirectory
import versionsort.VersionHelper

import scala.sys.process.Process
import scala.util.Try

name := "gosrc2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "com.lihaoyi"   %% "os-lib"            % Versions.osLib
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
goAstGenDlUrl := s"https://github.com/allsmog/joern/releases/download/go-astgen/v${goAstGenVersion.value}/"

def isCompatibleAstGen(command: Seq[String], requiredVersion: String): Boolean =
  Try(Process(command).!!.strip()).toOption.exists { installedVersion =>
    val normalizedInstalledVersion = installedVersion.stripPrefix("v")
    val normalizedRequiredVersion  = requiredVersion.stripPrefix("v")
    installedVersion != "unknown" &&
      VersionHelper.compare(normalizedInstalledVersion, normalizedRequiredVersion) >= 0
  }

def hasCompatibleAstGenVersion(goAstGenVersion: String): Boolean =
  isCompatibleAstGen(Seq("goastgen", "-version"), goAstGenVersion)

lazy val goAstGenBinaryNames = taskKey[Seq[String]]("goastgen binary names")
goAstGenBinaryNames := {
  val requiredVersion      = goAstGenVersion.value
  val allPlatforms         = sys.props.get("ALL_PLATFORMS").contains("TRUE")
  val bundledCurrentAstgen = baseDirectory.value / "bin" / "astgen" / goAstGenCurrentBinaryName.value
  if (hasCompatibleAstGenVersion(requiredVersion)) {
    Seq.empty
  } else if (allPlatforms) {
    Seq(GoAstgenWin, GoAstgenLinux, GoAstgenLinuxArm, GoAstgenMac, GoAstgenMacArm)
  } else if (isCompatibleAstGen(Seq(bundledCurrentAstgen.getAbsolutePath, "-version"), requiredVersion)) {
    Seq.empty
  } else {
    Environment.operatingSystem match {
      case Environment.OperatingSystemType.Windows =>
        Seq(GoAstgenWin)
      case Environment.OperatingSystemType.Linux =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86   => Seq(GoAstgenLinux)
          case Environment.ArchitectureType.ARMv8 => Seq(GoAstgenLinuxArm)
        }
      case Environment.OperatingSystemType.Mac =>
        Environment.architecture match {
          case Environment.ArchitectureType.X86   => Seq(GoAstgenMac)
          case Environment.ArchitectureType.ARMv8 => Seq(GoAstgenMacArm)
        }
      case Environment.OperatingSystemType.Unknown =>
        Seq(GoAstgenWin, GoAstgenLinux, GoAstgenLinuxArm, GoAstgenMac, GoAstgenMacArm)
    }
  }
}

lazy val goAstGenCurrentBinaryName = taskKey[String]("goastgen binary name for the current host")
goAstGenCurrentBinaryName := {
  Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows =>
      GoAstgenWin
    case Environment.OperatingSystemType.Linux =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86   => GoAstgenLinux
        case Environment.ArchitectureType.ARMv8 => GoAstgenLinuxArm
      }
    case Environment.OperatingSystemType.Mac =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86   => GoAstgenMac
        case Environment.ArchitectureType.ARMv8 => GoAstgenMacArm
      }
    case Environment.OperatingSystemType.Unknown =>
      GoAstgenLinux
  }
}

lazy val goAstGenBuildRust = taskKey[File]("Build local Rust goastgen and install it under bin/astgen")
goAstGenBuildRust := {
  val rustRoot       = baseDirectory.value / "rust"
  val rustBinaryName =
    if (Environment.operatingSystem == Environment.OperatingSystemType.Windows) "goastgen.exe" else "goastgen"
  val exitCode       = Process(Seq("cargo", "build", "--release", "--bin", "goastgen"), rustRoot).!
  if (exitCode != 0) {
    sys.error(s"cargo build failed with exit code $exitCode")
  }

  val builtBinary = rustRoot / "target" / "release" / rustBinaryName
  val goAstGenDir = baseDirectory.value / "bin" / "astgen"
  val targetFile  = goAstGenDir / goAstGenCurrentBinaryName.value
  goAstGenDir.mkdirs()
  IO.copyFile(builtBinary, targetFile, preserveLastModified = true)
  targetFile.setExecutable(true, false)
  streams.value.log.info(s"installed Rust goastgen to $targetFile")
  targetFile
}

lazy val goAstGenDlTask = taskKey[Unit](s"Download goastgen binaries")
goAstGenDlTask := {
  val goAstGenDir = baseDirectory.value / "bin" / "astgen"

  goAstGenBinaryNames.value.foreach { fileName =>
    val file = goAstGenDir / fileName
    DownloadHelper.ensureIsAvailable(s"${goAstGenDlUrl.value}$fileName", file)
    // permissions are lost during the download; need to set them manually
    file.setExecutable(true, false)
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(goAstGenDir, distDir, preserveExecutable = true)
}

Compile / compile := ((Compile / compile) dependsOn goAstGenDlTask).value

lazy val goAstGenSetAllPlatforms = taskKey[Unit](s"Set ALL_PLATFORMS")
goAstGenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(goAstGenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes(new java.util.jar.Attributes.Name("Go-AstGen-Version") -> goAstGenVersion.value)
