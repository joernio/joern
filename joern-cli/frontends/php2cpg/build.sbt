name := "php2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.1.3")

dependsOn(Projects.x2cpg)

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "ujson"             % "1.6.0",
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.apache.logging.log4j" % "log4j-slf4j-impl"  % Versions.log4j % Runtime,
  "io.circe"      %% "circe-core"        % "0.15.0-M1"
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
Global / onChangedBuildSource := ReloadOnSourceChanges
