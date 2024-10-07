import better.files
import com.typesafe.config.{Config, ConfigFactory}
import versionsort.VersionHelper

import scala.sys.process.stringToProcess
import scala.util.Try

name := "rubysrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val joernTypeStubsVersion = settingKey[String]("joern_type_stub version")
joernTypeStubsVersion := appProperties.value.getString("rubysrc2cpg.joern_type_stubs_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.apache.commons" % "commons-compress" % Versions.commonsCompress, // For unpacking Gems with `--download-dependencies`
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.antlr"      % "antlr4-runtime"    % Versions.antlr // TODO: Remove
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin, Antlr4Plugin)
// TODO Remove antlr stuff
Antlr4 / antlr4Version    := Versions.antlr
Antlr4 / antlr4GenVisitor := true
Antlr4 / javaSource       := (Compile / sourceManaged).value

lazy val astGenVersion = settingKey[String]("rubyastgen version")
astGenVersion := appProperties.value.getString("rubysrc2cpg.ruby_ast_gen_version")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

lazy val astGenDlUrl = settingKey[String]("astgen download url")
astGenDlUrl := s"https://github.com/joernio/ruby_ast_gen/releases/download/v${astGenVersion.value}/"

def hasCompatibleAstGenVersion(astGenVersion: String): Boolean = {
  Try("exec/ruby_ast_gen --version".!!).toOption.map(_.strip()) match {
    case Some(installedVersion) if installedVersion != "unknown" =>
      VersionHelper.compare(installedVersion, astGenVersion) >= 0
    case _ => false
  }
}

lazy val astGenDlTask = taskKey[Unit](s"Download astgen binaries")
astGenDlTask := {
  if (hasCompatibleAstGenVersion(astGenVersion.value)) {
    Seq.empty
  } else {
    val astGenDir = baseDirectory.value / "bin" / "astgen"
    astGenDir.mkdirs()
    val gemName = s"ruby_ast_gen-${astGenVersion.value}.gem"
    val gemFullPath  = astGenDir / gemName
    val gemNoVersionFullPath  = astGenDir / s"${gemName.stripSuffix(s"-${astGenVersion.value}.gem")}.gem"
    val unpackedGemNoVersion = gemName.stripSuffix(s"-${astGenVersion.value}.gem")
    val unpackedGemNoVersionFullPath =  astGenDir / unpackedGemNoVersion
    println("Something: " + unpackedGemNoVersionFullPath)
    //  We set this up so that the unpacked version is what the download helper aims to keep available
    DownloadHelper.ensureIsAvailable(s"${astGenDlUrl.value}$gemName", unpackedGemNoVersionFullPath)
    // We need to rename the file, unpack, then clean up again
    println(s"$unpackedGemNoVersionFullPath ${unpackedGemNoVersionFullPath.exists()}")
    unpackedGemNoVersionFullPath.renameTo(gemNoVersionFullPath)
    println(s"Renamed to ${gemNoVersionFullPath}")
    val code = s"gem unpack $gemNoVersionFullPath --target $astGenDir" !

    gemNoVersionFullPath.renameTo(unpackedGemNoVersionFullPath)
    gemFullPath.delete()
    gemNoVersionFullPath.delete()
    if (code != 0) {
      println("Unable to unpack AST generator Gem. Please make sure Ruby is installed.")
    } else {
      val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
      distDir.mkdirs()
      IO.copyDirectory(astGenDir, distDir)

      // permissions are lost during the download; need to set them manually
      astGenDir.listFiles().foreach(_.setExecutable(true, false))
      distDir.listFiles().foreach(_.setExecutable(true, false))
    }
  }
}

Compile / compile := ((Compile / compile) dependsOn astGenDlTask).value

lazy val joernTypeStubsDlUrl = settingKey[String]("joern_type_stubs download url")
joernTypeStubsDlUrl := s"https://github.com/joernio/joern-type-stubs/releases/download/v${joernTypeStubsVersion.value}/"

lazy val joernTypeStubsDlTask = taskKey[Unit]("Download joern-type-stubs")
joernTypeStubsDlTask := {
  val joernTypeStubsDir = baseDirectory.value / "type_stubs"
  val fileName = "rubysrc_builtin_types.zip"
  val shaFileName = s"$fileName.sha512"

  joernTypeStubsDir.mkdir()

  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$fileName", joernTypeStubsDir / fileName)
  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$shaFileName", joernTypeStubsDir / shaFileName)

  val typeStubsFile = better.files.File(joernTypeStubsDir.getAbsolutePath) / fileName
  val checksumFile = better.files.File(joernTypeStubsDir.getAbsolutePath)  / shaFileName

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

Universal / packageName := name.value
Universal / topLevelDirectory := None
