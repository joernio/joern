import sbt.BareBuildSyntax.dependsOn
import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "swiftsrc2cpg"

dependsOn(
  Projects.dataflowengineoss % "test->test",
  Projects.x2cpg             % "compile->compile;test->test",
  Projects.linterRules       % ScalafixConfig
)

lazy val astGenVersion = settingKey[String]("astgen version")
astGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "swiftsrc2cpg.astgen_version")

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"  %% "upickle"           % Versions.upickle,
  // we want to use also Google Gson for its streaming abilities for very large Json files:
  "com.google.code.gson" % "gson" % Versions.gson,
  // to handle property list files of various formats (i.e., binary and plain XML)
  "com.googlecode.plist" % "dd-plist"  % "1.28",
  "org.scalatest"       %% "scalatest" % Versions.scalatest % Test,
  "org.scala-lang.modules" %% "scala-parallel-collections" % Versions.scalaParallel
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val AstgenWin      = "SwiftAstGen-win.exe"
lazy val AstgenLinux    = "SwiftAstGen-linux"
lazy val AstgenLinuxArm = "SwiftAstGen-linux-arm64"
lazy val AstgenMac      = "SwiftAstGen-mac"

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/swift-astgen/v${astGenVersion.value}/"

/** Probed once per build load (settings are re-evaluated on load/reload), so that the result participates in the
  * cache key of the cached `astGenDownloadTask` below instead of being a hidden side effect.
  */
lazy val compatibleAstGenOnPath = settingKey[Boolean]("compatible astgen available on PATH")
compatibleAstGenOnPath := DownloadHelper.hasCompatibleVersionOnPath("SwiftAstGen", astGenVersion.value)

lazy val astGenBinaryNames = settingKey[Seq[String]]("astgen binary names")
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
  Package.ManifestAttributes("Swift-AstGen-Version" -> astGenVersion.value)
