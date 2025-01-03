import better.files
import com.typesafe.config.{Config, ConfigFactory}
import versionsort.VersionHelper

import java.net.URI
import scala.sys.process.stringToProcess
import scala.util.{Failure, Success, Try}

name := "rubysrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val joernTypeStubsVersion = settingKey[String]("joern_type_stub version")
joernTypeStubsVersion := appProperties.value.getString("rubysrc2cpg.joern_type_stubs_version")

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "org.apache.commons" % "commons-compress" % Versions.commonsCompress, // For unpacking Gems with `--download-dependencies`
  "org.jruby"      % "jruby-complete" % Versions.jRuby,
  "org.scalatest" %% "scalatest"      % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

lazy val astGenVersion = settingKey[String]("`ruby_ast_gen` version")
astGenVersion := appProperties.value.getString("rubysrc2cpg.ruby_ast_gen_version")

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/ruby_ast_gen/releases/download/v${astGenVersion.value}/"

def hasCompatibleAstGenVersion(astGenBaseDir: File, astGenVersion: String): Boolean = {
  val versionFile = astGenBaseDir / "lib" / "ruby_ast_gen" / "version.rb"
  if (!versionFile.exists) return false
  val versionPattern = "VERSION = \"([0-9]+\\.[0-9]+\\.[0-9]+)\"".r
  versionPattern.findFirstIn(IO.read(versionFile)) match {
    case Some(versionString) =>
      // Regex group matching doesn't appear to work in SBT
      val version = versionString.stripPrefix("VERSION = \"").stripSuffix("\"")
      version == astGenVersion
    case _ => false
  }
}

lazy val astGenResourceTask = taskKey[Seq[File]](s"Download `ruby_ast_gen` and package this under `resources`")
astGenResourceTask := {
  val targetDir           = baseDirectory.value / "src" / "main" / "resources"
  val gemName             = s"ruby_ast_gen_v${astGenVersion.value}.zip"
  val compressGemPath     = targetDir / gemName
  val unpackedGemFullPath = targetDir / gemName.stripSuffix(s"_v${astGenVersion.value}.zip")
  if (!hasCompatibleAstGenVersion(unpackedGemFullPath, astGenVersion.value)) {
    if (unpackedGemFullPath.exists()) IO.delete(unpackedGemFullPath)
    val url = s"${astGenDlUrl.value}$gemName"
    sbt.io.Using.urlInputStream(new URI(url).toURL) { inputStream =>
      sbt.IO.transfer(inputStream, compressGemPath)
    }
    IO.unzip(compressGemPath, unpackedGemFullPath)
    IO.delete(compressGemPath)
  }
  (unpackedGemFullPath ** "*").get.filter(_.isFile)
}

Compile / resourceGenerators += astGenResourceTask

lazy val joernTypeStubsDlUrl = settingKey[String]("joern_type_stubs download url")
joernTypeStubsDlUrl := s"https://github.com/joernio/joern-type-stubs/releases/download/v${joernTypeStubsVersion.value}/"

lazy val joernTypeStubsDlTask = taskKey[Unit]("Download joern-type-stubs")
joernTypeStubsDlTask := {
  val joernTypeStubsDir = baseDirectory.value / "type_stubs"
  val fileName          = "rubysrc_builtin_types.zip"
  val shaFileName       = s"$fileName.sha512"

  joernTypeStubsDir.mkdir()

  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$fileName", joernTypeStubsDir / fileName)
  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$shaFileName", joernTypeStubsDir / shaFileName)

  val typeStubsFile = better.files.File(joernTypeStubsDir.getAbsolutePath) / fileName
  val checksumFile  = better.files.File(joernTypeStubsDir.getAbsolutePath) / shaFileName

  val typestubsSha = typeStubsFile.sha512

  // Checksum file must contain exactly 1 line, if more or less we automatically fail.
  if (checksumFile.lineIterator.size != 1) {
    throw new IllegalStateException("Checksum File should only have 1 line")
  }

  // Checksum from terminal adds the filename to the line, so we split on whitespace to get the checksum
  // separate from the filename
  if (checksumFile.lineIterator.next().split("\\s+")(0).toUpperCase != typestubsSha) {
    throw new Exception("Checksums do not match for type stubs!")
  }

  val distDir = (Universal / stagingDirectory).value / "type_stubs"
  distDir.mkdirs()
  IO.copyDirectory(joernTypeStubsDir, distDir)
}

Compile / compile := ((Compile / compile) dependsOn joernTypeStubsDlTask).value

Universal / packageName       := name.value
Universal / topLevelDirectory := None

/** write the astgen version to the manifest for downstream usage */
Compile / packageBin / packageOptions +=
  Package.ManifestAttributes(new java.util.jar.Attributes.Name("Ruby-AstGen-Version") -> astGenVersion.value)
