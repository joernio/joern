name := "javasrc2cpg"
organization := "io.joern"

scalaVersion := "2.13.6"

val scalatestVersion = "3.1.1"

trapExit := false

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg"              % Versions.cpg,
  "io.shiftleft"  %% "dataflowengineoss"        % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg-tests"        % Versions.cpg       % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.22.1",
  "org.scalatest" %% "scalatest"                % scalatestVersion % Test
)

developers := List(
  Developer(
    "fabsx00",
    "Fabian Yamaguchi",
    "fabs@shiftleft.io",
    url("https://github.com/fabsx00")
  )
)

enablePlugins(JavaAppPackaging)

Global / onChangedBuildSource := ReloadOnSourceChanges
       	 
