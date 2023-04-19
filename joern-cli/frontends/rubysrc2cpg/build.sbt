name := "rubysrc2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.2.2")

dependsOn(
  Projects.dataflowengineoss,
  Projects.x2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "io.shiftleft"               %% "codepropertygraph" % Versions.cpg,
  "org.scalatest"              %% "scalatest"         % Versions.scalatest % Test,
  "org.antlr"                   % "antlr4-runtime"    % Versions.antlr
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(
  JavaAppPackaging,
  LauncherJarPlugin,
  Antlr4Plugin
)

Antlr4 / antlr4PackageName := Some("io.joern.rubysrc2cpg.parser")
Antlr4 / antlr4Version := Versions.antlr
Antlr4 / antlr4GenVisitor := true
Antlr4 / javaSource := (Compile / sourceManaged).value
