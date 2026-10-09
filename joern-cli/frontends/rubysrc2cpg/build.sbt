import sbt.BareBuildSyntax.dependsOn
import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "rubysrc2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

lazy val joernTypeStubsVersion = settingKey[String]("joern_type_stub version")
joernTypeStubsVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "rubysrc2cpg.joern_type_stubs_version")

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "org.apache.commons" % "commons-compress" % Versions.commonsCompress, // For unpacking Gems with `--download-dependencies`
  "org.jruby"      % "jruby-complete" % Versions.jRuby,
  "org.scalatest" %% "scalatest"      % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val astGenVersion = settingKey[String]("`ruby_ast_gen` version")
astGenVersion := DownloadHelper.appConfigVersion((Compile / resourceDirectory).value, "rubysrc2cpg.ruby_ast_gen_version")

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/astgen-monorepo/releases/download/ruby-astgen/v${astGenVersion.value}/"

lazy val astGenPlatformSuffix = settingKey[String]("platform suffix for ruby_ast_gen archive")
astGenPlatformSuffix := {
  (buildOperatingSystem.value, buildArchitecture.value) match {
    case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)       => "macos"
    case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)     => "macos-arm"
    case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)     => "linux"
    case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8)   => "linux-arm"
    case (Environment.OperatingSystemType.Windows, Environment.ArchitectureType.X86)   => "win"
    case (Environment.OperatingSystemType.Windows, Environment.ArchitectureType.ARMv8) => "win-arm"
    case _ => "linux"
  }
}

def hasCompatibleAstGenVersion(astGenBaseDir: File, astGenVersion: String): Boolean = {
  val versionFile = astGenBaseDir / "lib" / "ruby_ast_gen" / "version.rb"
  if (!versionFile.exists) return false
  // A partial or stale unpack may contain the version file but lack the vendored gems
  val bundleBase = astGenBaseDir / "vendor" / "bundle" / "jruby"
  val hasGems = Option(bundleBase.listFiles()).getOrElse(Array.empty[File]).exists { abiDir =>
    Option((abiDir / "gems").listFiles()).getOrElse(Array.empty[File]).exists(_.getName.startsWith("ast-"))
  }
  if (!hasGems) return false
  val versionPattern = "VERSION = \"([0-9]+\\.[0-9]+\\.[0-9]+)\"".r
  versionPattern.findFirstIn(IO.read(versionFile)) match {
    case Some(versionString) =>
      // Regex group matching doesn't appear to work in SBT
      val version = versionString.stripPrefix("VERSION = \"").stripSuffix("\"")
      version == astGenVersion
    case _ => false
  }
}

/** Downloads the ruby_ast_gen zip into the project output directory. Cached by sbt: the cache key includes
  * `astGenDlUrl` (and thus `astGenVersion`) and `astGenPlatformSuffix`, so a version bump re-triggers the
  * download. The zip is a declared output, so on a cache hit sbt re-materializes it without hitting the network.
  */
lazy val astGenDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download ruby_ast_gen zip")
astGenDownloadTask := DownloadHelper
  .downloadArtifacts(
    target.value / "astgen-download",
    astGenDlUrl.value,
    Seq(s"ruby_ast_gen-${astGenPlatformSuffix.value}_v${astGenVersion.value}.zip"),
    fileConverter.value,
    executable = false
  )
  .map { vf => Def.declareOutput(vf); vf }

lazy val astGenResourceTask = taskKey[Seq[File]](s"Unpack `ruby_ast_gen` and package this under `resources`")
astGenResourceTask := Def.uncached {
  val conv                = fileConverter.value
  val compressGemPath     = astGenDownloadTask.value.map(ref => conv.toPath(ref).toFile).head
  val unpackedGemFullPath = baseDirectory.value / "src" / "main" / "resources" / "ruby_ast_gen"
  if (!hasCompatibleAstGenVersion(unpackedGemFullPath, astGenVersion.value)) {
    if (unpackedGemFullPath.exists()) IO.delete(unpackedGemFullPath)
    IO.unzip(compressGemPath, unpackedGemFullPath)
  }
  (unpackedGemFullPath ** "*").get().filter(_.isFile)
}

Compile / resourceGenerators += astGenResourceTask

lazy val joernTypeStubsDlUrl = settingKey[String]("joern_type_stubs download url")
joernTypeStubsDlUrl := s"https://github.com/joernio/joern-type-stubs/releases/download/v${joernTypeStubsVersion.value}/"

/** Downloads the joern-type-stubs zip and its checksum into the project output directory (cached by sbt, see
  * above). The checksum is verified before the task completes, so a corrupt download fails the task and is
  * never cached.
  */
lazy val joernTypeStubsDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download joern-type-stubs")
joernTypeStubsDownloadTask := {
  val conv        = fileConverter.value
  val fileName    = "rubysrc_builtin_types.zip"
  val shaFileName = s"$fileName.sha512"
  val refs = DownloadHelper.downloadArtifacts(
    target.value / "type-stubs-download",
    joernTypeStubsDlUrl.value,
    Seq(fileName, shaFileName),
    conv,
    executable = false
  )

  val Seq(typeStubsFile, checksumFile) = refs.map(ref => better.files.File(conv.toPath(ref).toFile.getAbsolutePath))
  val typestubsSha                     = typeStubsFile.sha512

  // Checksum file must contain exactly 1 line, if more or less we automatically fail.
  if (checksumFile.lineIterator.size != 1) {
    throw new IllegalStateException("Checksum File should only have 1 line")
  }

  // Checksum from terminal adds the filename to the line, so we split on whitespace to get the checksum
  // separate from the filename
  if (checksumFile.lineIterator.next().split("\\s+")(0).toUpperCase != typestubsSha) {
    throw new Exception("Checksums do not match for type stubs!")
  }

  refs.map { vf => Def.declareOutput(vf); vf }
}

lazy val joernTypeStubsStageTask =
  taskKey[Unit]("Stage joern-type-stubs into type_stubs/ and the Universal staging directory")
joernTypeStubsStageTask := Def.uncached {
  DownloadHelper.stageArtifacts(
    joernTypeStubsDownloadTask.value,
    fileConverter.value,
    Seq(baseDirectory.value / "type_stubs", (Universal / stagingDirectory).value / "type_stubs"),
    executable = false
  )
}

Compile / compile := Def.uncached { ((Compile / compile).dependsOn(joernTypeStubsStageTask)).value }

Compile / packageSrc / mappings ~= (_.distinctBy(_._2))

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes("Ruby-AstGen-Version" -> astGenVersion.value)
