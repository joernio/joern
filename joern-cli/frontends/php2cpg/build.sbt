import scala.sys.process._
import scala.util.Properties.isWin

name := "php2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.3.0")

val phpParserVersion = "4.15.6"
val phpParserBinName = "php-parser.phar"
val phpParserDlUrl   = s"https://github.com/joernio/PHP-Parser/releases/download/v$phpParserVersion/$phpParserBinName"
dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "ujson"             % "2.0.0",
  "com.lihaoyi"   %% "upickle"           % "2.0.0",
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "io.circe"      %% "circe-core"        % "0.15.0-M1"
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

lazy val phpParseInstallTask = taskKey[Unit]("Install PHP-Parse using PHP Composer")
phpParseInstallTask := {
  val phpBinDir = baseDirectory.value / "bin"
  if (!(phpBinDir / phpParserBinName).exists) {
    IO.createDirectory(phpBinDir)
    val downloadedFile = SimpleCache.downloadMaybe(phpParserDlUrl)
    IO.copyFile(downloadedFile, phpBinDir / phpParserBinName)
  }

  val distDir = (Universal / stagingDirectory).value / "bin"
  distDir.mkdirs()
  IO.copyDirectory(phpBinDir, distDir)
}

cleanFiles ++= Seq(
  // left to clean legacy representation
  baseDirectory.value / "bin" / "PHP-Parser",
  baseDirectory.value / "bin" / phpParserBinName,
  (Universal / stagingDirectory).value / "bin" / "PHP-Parser",
  (Universal / stagingDirectory).value / "bin" / phpParserBinName
)

Compile / compile := ((Compile / compile) dependsOn phpParseInstallTask).value

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
Global / onChangedBuildSource := ReloadOnSourceChanges
