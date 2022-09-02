import scala.sys.process._
import scala.util.Properties.isWin

name := "php2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.1.3")

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "ujson"             % "2.0.0",
  "com.lihaoyi"   %% "upickle"           % "2.0.0",
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.apache.logging.log4j" % "log4j-slf4j-impl"  % Versions.log4j % Runtime,
  "io.circe"      %% "circe-core"        % "0.15.0-M1"
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

// lazy val phpParseInstallTask = taskKey[Unit]("Install PHP-Parse using PHP Composer")
// phpParseInstallTask := {
//   val phpBinDir = baseDirectory.value / "bin"
//   val phpParseBinary = phpBinDir / "vendor" / "bin" / "php-parse"
//   if (!phpParseBinary.exists) {
//     val installSciptPath =
//       if (isWin)
//         (phpBinDir / "installdeps.bat").getPath
//       else
//         (phpBinDir / "installdeps.sh").getPath
//     Process(installSciptPath, phpBinDir) !
//   }
// 
//   val distDir = (Universal / stagingDirectory).value / "bin"
//   distDir.mkdirs()
//   IO.copyDirectory(phpBinDir, distDir)
// }
// 
// Compile / compile := ((Compile / compile) dependsOn phpParseInstallTask).value

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
Global / onChangedBuildSource := ReloadOnSourceChanges
