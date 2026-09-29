import sbt.BareBuildSyntax.dependsOn
import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "abap2cpg"

dependsOn(
  Projects.dataflowengineoss % "compile->compile;test->test",
  Projects.x2cpg             % "compile->compile;test->test",
  Projects.linterRules       % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"   %% "ujson"             % Versions.upickle,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val abapgenVersion = settingKey[String]("abapgen version")
abapgenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "abap2cpg.abapgen_version")

// Released binary names (produced by the abap-astgen-release.yml workflow in
// joernio/astgen-monorepo after pkg output is renamed).
lazy val AbapgenWinX86   = "abapgen-win.exe"
lazy val AbapgenWinArm   = "abapgen-win-arm.exe"
lazy val AbapgenLinuxX86 = "abapgen-linux"
lazy val AbapgenLinuxArm = "abapgen-linux-arm"
lazy val AbapgenMacX86   = "abapgen-macos"
lazy val AbapgenMacArm   = "abapgen-macos-arm"

lazy val abapgenDlUrl = settingKey[String]("abapgen download url")
abapgenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/abap-astgen/v${abapgenVersion.value}/"

lazy val abapgenBinaryNames = settingKey[Seq[String]]("abapgen binary names for current platform")
abapgenBinaryNames := DownloadHelper.platformBinaries(buildOperatingSystem.value, buildArchitecture.value)(
  windows = Some(AbapgenWinX86),
  windowsArm = Some(AbapgenWinArm),
  linux = Some(AbapgenLinuxX86),
  linuxArm = Some(AbapgenLinuxArm),
  mac = Some(AbapgenMacX86),
  macArm = Some(AbapgenMacArm)
)

lazy val abapgenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download abapgen binaries from joernio/astgen-monorepo release")
abapgenDownloadTask := DownloadHelper
  .downloadArtifacts(target.value / "abapgen-download", abapgenDlUrl.value, abapgenBinaryNames.value, fileConverter.value)
  .map { vf => Def.declareOutput(vf); vf }

lazy val abapgenStageTask = taskKey[Unit]("Stage abapgen binaries into bin/astgen and the Universal staging directory")
abapgenStageTask := Def.uncached {
  DownloadHelper.stageArtifacts(
    abapgenDownloadTask.value,
    fileConverter.value,
    Seq(baseDirectory.value / "bin" / "astgen", (Universal / stagingDirectory).value / "bin" / "astgen")
  )
}

Compile / compile := Def.uncached { ((Compile / compile).dependsOn(abapgenStageTask)).value }

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the abapgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes("Abap-AstGen-Version" -> abapgenVersion.value)
