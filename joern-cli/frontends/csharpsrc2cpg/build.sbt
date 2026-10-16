import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

lazy val astGenVersion = settingKey[String]("dotnetastgen version")

lazy val AstgenWin      = "dotnetastgen-win.exe"
lazy val AstgenLinux    = "dotnetastgen-linux"
lazy val AstgenLinuxArm = "dotnetastgen-linux-arm64"
lazy val AstgenMac      = "dotnetastgen-macos"

lazy val astGenDlUrl = settingKey[String]("astgen download url")

/** Probed once per build load (settings are re-evaluated on load/reload), so that the result participates in the
  * cache key of the cached `astGenDownloadTask` below instead of being a hidden side effect.
  */
lazy val compatibleAstGenOnPath = settingKey[Boolean]("compatible astgen available on PATH")

lazy val astGenBinaryNames = settingKey[Seq[String]]("astgen binary names")

lazy val astGenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download astgen binaries")

lazy val astGenStageTask = taskKey[Unit]("Stage astgen binaries into bin/astgen and the Universal staging directory")

lazy val csharpsrc2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "csharpsrc2cpg",
    astGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "csharpsrc2cpg.dotnetastgen_version"),

    libraryDependencies ++= Seq(
      "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
      "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
    ),

    Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value),

    compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g"),
    astGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/dotnet-astgen/v${astGenVersion.value}/",
    compatibleAstGenOnPath := DownloadHelper.hasCompatibleVersionOnPath("dotnetastgen", astGenVersion.value),
    astGenBinaryNames := {
      if (compatibleAstGenOnPath.value) {
        Seq.empty
      } else {
        DownloadHelper.platformBinaries(buildOperatingSystem.value, buildArchitecture.value)(
          windows = Some(AstgenWin),
          linux = Some(AstgenLinux),
          linuxArm = Some(AstgenLinuxArm),
          mac = Some(AstgenMac)
        )
      }
    },
    astGenDownloadTask := DownloadHelper
      .downloadArtifacts(target.value / "astgen-download", astGenDlUrl.value, astGenBinaryNames.value, fileConverter.value)
      .map { vf => Def.declareOutput(vf); vf },
    astGenStageTask := Def.uncached {
      DownloadHelper.stageArtifacts(
        astGenDownloadTask.value,
        fileConverter.value,
        Seq(baseDirectory.value / "bin" / "astgen", (Universal / stagingDirectory).value / "bin" / "astgen")
      )
    },

    Compile / compile := Def.uncached { ((Compile / compile).dependsOn(astGenStageTask)).value },

    Universal / packageName       := name.value,
    Universal / topLevelDirectory := None,

    /** write the astgen version to the manifest for downstream usage */
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes("DotNet-AstGen-Version" -> astGenVersion.value)
  )
