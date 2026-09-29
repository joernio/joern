import sbt.BareBuildSyntax.dependsOn
import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "rust2cpg"

dependsOn(
  Projects.dataflowengineoss % "test->test",
  Projects.x2cpg             % "compile->compile;test->test",
  Projects.linterRules       % ScalafixConfig
)

lazy val astGenVersion = settingKey[String]("rust_ast_gen version")
astGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "rust2cpg.rust_ast_gen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"   %% "upickle"           % Versions.upickle,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWin      = "rust_ast_gen-win.exe"
lazy val AstgenWinArm   = "rust_ast_gen-win-arm.exe"
lazy val AstgenLinux    = "rust_ast_gen-linux"
lazy val AstgenLinuxArm = "rust_ast_gen-linux-arm"
lazy val AstgenMac      = "rust_ast_gen-macos"
lazy val AstgenMacArm   = "rust_ast_gen-macos-arm"

lazy val astGenDlUrl = settingKey[String]("rust_ast_gen download url")
astGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/rust-astgen/v${astGenVersion.value}/"

/** Probed once per build load (settings are re-evaluated on load/reload), so that the result participates in the
  * cache key of the cached `astGenDownloadTask` below instead of being a hidden side effect.
  */
lazy val compatibleAstGenOnPath = settingKey[Boolean]("compatible rust_ast_gen available on PATH")
compatibleAstGenOnPath := DownloadHelper.hasCompatibleVersionOnPath(
  "rust_ast_gen",
  astGenVersion.value,
  versionPrefix = "rust_ast_gen "
)

lazy val astGenBinaryNames = settingKey[Seq[String]]("rust_ast_gen binary names")
astGenBinaryNames := {
  if (compatibleAstGenOnPath.value) {
    Seq.empty
  } else {
    DownloadHelper.platformBinaries(buildOperatingSystem.value, buildArchitecture.value)(
      windows = Some(AstgenWin),
      windowsArm = Some(AstgenWinArm),
      linux = Some(AstgenLinux),
      linuxArm = Some(AstgenLinuxArm),
      mac = Some(AstgenMac),
      macArm = Some(AstgenMacArm)
    )
  }
}

lazy val astGenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download rust_ast_gen binaries")
astGenDownloadTask := DownloadHelper
  .downloadArtifacts(target.value / "astgen-download", astGenDlUrl.value, astGenBinaryNames.value, fileConverter.value)
  .map { vf => Def.declareOutput(vf); vf }

lazy val astGenStageTask = taskKey[Unit]("Stage rust_ast_gen binaries into bin/astgen and the Universal staging directory")
astGenStageTask := Def.uncached {
  DownloadHelper.stageArtifacts(
    astGenDownloadTask.value,
    fileConverter.value,
    Seq(baseDirectory.value / "bin" / "astgen", (Universal / stagingDirectory).value / "bin" / "astgen")
  )
}

Compile / compile := Def.uncached { ((Compile / compile).dependsOn(astGenStageTask)).value }

lazy val rustNodeSyntaxDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download RustNodeSyntax.scala")
rustNodeSyntaxDownloadTask := DownloadHelper
  .downloadArtifacts(
    (Compile / sourceManaged).value / "io" / "joern" / "rust2cpg" / "parser",
    astGenDlUrl.value,
    Seq("RustNodeSyntax.scala"),
    fileConverter.value,
    executable = false
  )
  .map { vf => Def.declareOutput(vf); vf }

lazy val rustNodeSyntaxSourceTask = taskKey[Seq[File]]("RustNodeSyntax.scala as generated source")
rustNodeSyntaxSourceTask := Def.uncached {
  rustNodeSyntaxDownloadTask.value.map(ref => fileConverter.value.toPath(ref).toFile)
}

Compile / sourceGenerators += rustNodeSyntaxSourceTask

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes("Rust-AstGen-Version" -> astGenVersion.value)
