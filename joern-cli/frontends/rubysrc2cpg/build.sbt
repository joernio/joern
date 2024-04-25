import better.files
import com.typesafe.config.{Config, ConfigFactory}

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
  "org.antlr"      % "antlr4-runtime"    % Versions.antlr
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin, Antlr4Plugin)

Antlr4 / antlr4Version    := Versions.antlr
Antlr4 / antlr4GenVisitor := true
Antlr4 / javaSource       := (Compile / sourceManaged).value

lazy val joernTypeStubsDlUrl = settingKey[String]("joern_type_stubs download url")
joernTypeStubsDlUrl := s"https://github.com/joernio/joern-type-stubs/releases/download/v${joernTypeStubsVersion.value}/"

lazy val joernTypeStubsDlTask = taskKey[Unit]("Download joern-type-stubs")
joernTypeStubsDlTask := {
  val joernTypeStubsDir = baseDirectory.value / "src" / "main" / "resources"
  val fileName = "rubysrc_builtin_types.zip"
  val shaFileName = s"${fileName}.sha512"

  joernTypeStubsDir.mkdir()

  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$fileName", joernTypeStubsDir / fileName)
  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$shaFileName", joernTypeStubsDir / shaFileName)
}

Compile / compile := ((Compile / compile) dependsOn joernTypeStubsDlTask).value
