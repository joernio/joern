import scala.sys.process._
import scala.util.Properties.isWin

name := "php2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.2.2")

val phpParserVersion = "4.15.4-charfix.1"
val phpParserDlUrl  = s"https://github.com/joernio/PHP-Parser/archive/refs/tags/$phpParserVersion.zip"

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
  val phpBinDir      = baseDirectory.value / "bin"
  val phpParseDir = phpBinDir / "PHP-Parser"
  if (!(phpParseDir / "bin").exists) {
    IO.createDirectory(phpParseDir)
    val downloadedFile = SimpleCache.downloadMaybe(phpParserDlUrl)
    IO.withTemporaryDirectory { tempDir =>
      IO.unzip(downloadedFile, tempDir)

      val srcDir = tempDir / s"PHP-Parser-$phpParserVersion"
      IO.copyDirectory(srcDir, phpParseDir)
    }
  }

  val distDir = (Universal / stagingDirectory).value / "bin"
  distDir.mkdirs()
  IO.copyDirectory(phpBinDir, distDir)

  // permissions are lost during the download; need to set them manually
  (phpParseDir / "bin").listFiles().foreach(_.setExecutable(true, false))
  (distDir / "PHP-Parser" / "bin").listFiles().foreach(_.setExecutable(true, false))
}

cleanFiles ++= Seq(
  baseDirectory.value / "bin" / "PHP-Parser",
  (Universal / stagingDirectory).value / "bin" / "PHP-Parser"
)

Compile / compile := ((Compile / compile) dependsOn phpParseInstallTask).value

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
Global / onChangedBuildSource := ReloadOnSourceChanges
