import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

lazy val goAstGenVersion = settingKey[String]("goastgen version")

lazy val GoAstgenWin      = "goastgen-windows.exe"
lazy val GoAstgenLinux    = "goastgen-linux"
lazy val GoAstgenLinuxArm = "goastgen-linux-arm64"
lazy val GoAstgenMac      = "goastgen-macos"
lazy val GoAstgenMacArm   = "goastgen-macos-arm64"

lazy val goAstGenDlUrl = settingKey[String]("goastgen download url")

/** Probed once per build load (settings are re-evaluated on load/reload), so that the result participates in the
  * cache key of the cached `goAstGenDownloadTask` below instead of being a hidden side effect.
  */
lazy val compatibleGoAstGenOnPath = settingKey[Boolean]("compatible goastgen available on PATH")

lazy val goAstGenBinaryNames = settingKey[Seq[String]]("goastgen binary names")

lazy val goAstGenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download goastgen binaries")

lazy val goAstGenStageTask = taskKey[Unit]("Stage goastgen binaries into bin/astgen and the Universal staging directory")

lazy val gosrc2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "gosrc2cpg",

    libraryDependencies ++= Seq(
      "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
      "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
      "com.lihaoyi"   %% "os-lib"            % Versions.osLib
    ),
    goAstGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "gosrc2cpg.goastgen_version"),
    goAstGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/go-astgen/v${goAstGenVersion.value}/",
    compatibleGoAstGenOnPath := DownloadHelper.hasCompatibleVersionOnPath("goastgen", goAstGenVersion.value, versionFlag = "-version"),
    goAstGenBinaryNames := {
      if (compatibleGoAstGenOnPath.value) {
        Seq.empty
      } else {
        DownloadHelper.platformBinaries(buildOperatingSystem.value, buildArchitecture.value)(
          windows = Some(GoAstgenWin),
          linux = Some(GoAstgenLinux),
          linuxArm = Some(GoAstgenLinuxArm),
          mac = Some(GoAstgenMac),
          macArm = Some(GoAstgenMacArm)
        )
      }
    },
    goAstGenDownloadTask := DownloadHelper
      .downloadArtifacts(target.value / "astgen-download", goAstGenDlUrl.value, goAstGenBinaryNames.value, fileConverter.value)
      .map { vf => Def.declareOutput(vf); vf },
    goAstGenStageTask := Def.uncached {
      DownloadHelper.stageArtifacts(
        goAstGenDownloadTask.value,
        fileConverter.value,
        Seq(baseDirectory.value / "bin" / "astgen", (Universal / stagingDirectory).value / "bin" / "astgen")
      )
    },

    Compile / compile := Def.uncached { ((Compile / compile).dependsOn(goAstGenStageTask)).value },

    /** write the astgen version to the manifest for downstream usage */
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes("Go-AstGen-Version" -> goAstGenVersion.value)
  )
