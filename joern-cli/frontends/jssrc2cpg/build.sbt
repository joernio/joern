import sbt.BareBuildSyntax.dependsOn
import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "jssrc2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

lazy val astGenVersion = settingKey[String]("astgen version")
astGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "jssrc2cpg.astgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWinAmd64   = "astgen-win.exe"
lazy val AstgenWinArmV8   = "astgen-win-arm.exe"
lazy val AstgenLinuxAmd64 = "astgen-linux"
lazy val AstgenLinuxArmV8 = "astgen-linux-arm"
lazy val AstgenMacAmd64   = "astgen-macos"
lazy val AstgenMacArmV8   = "astgen-macos-arm"

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/javascript-astgen/v${astGenVersion.value}/"

/** Probed once per build load (settings are re-evaluated on load/reload), so that the result participates in the
  * cache key of the cached `astGenDownloadTask` below instead of being a hidden side effect.
  */
lazy val compatibleAstGenOnPath = settingKey[Boolean]("compatible astgen available on PATH")
compatibleAstGenOnPath := DownloadHelper.hasCompatibleVersionOnPath("astgen", astGenVersion.value)

lazy val astGenBinaryNames = settingKey[Seq[String]]("astgen binary names")
astGenBinaryNames := {
  if (compatibleAstGenOnPath.value) {
    Seq.empty
  } else {
    DownloadHelper.platformBinaries(buildOperatingSystem.value, buildArchitecture.value)(
      windows = Some(AstgenWinAmd64),
      windowsArm = Some(AstgenWinArmV8),
      linux = Some(AstgenLinuxAmd64),
      linuxArm = Some(AstgenLinuxArmV8),
      mac = Some(AstgenMacAmd64),
      macArm = Some(AstgenMacArmV8)
    )
  }
}

lazy val astGenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download astgen binaries")
astGenDownloadTask := DownloadHelper
  .downloadArtifacts(target.value / "astgen-download", astGenDlUrl.value, astGenBinaryNames.value, fileConverter.value)
  .map { vf => Def.declareOutput(vf); vf }

lazy val astGenStageTask = taskKey[Unit]("Stage astgen binaries into bin/astgen and the Universal staging directory")
astGenStageTask := Def.uncached {
  DownloadHelper.stageArtifacts(
    astGenDownloadTask.value,
    fileConverter.value,
    Seq(baseDirectory.value / "bin" / "astgen", (Universal / stagingDirectory).value / "bin" / "astgen")
  )
}

Compile / compile := Def.uncached { ((Compile / compile).dependsOn(astGenStageTask)).value }

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes("JS-AstGen-Version" -> astGenVersion.value)
