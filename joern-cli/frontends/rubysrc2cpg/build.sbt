name := "rubysrc2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.2.2")

dependsOn(
  Projects.dataflowengineoss,
  Projects.x2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "io.shiftleft"               %% "codepropertygraph" % Versions.cpg,
  "org.scalatest"              %% "scalatest"         % Versions.scalatest % Test
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
