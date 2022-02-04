name := "jimple2cpg"

scalaVersion := "2.13.8"

val sootVersion      = "4.2.1"
val slf4jVersion     = "1.7.35"
val scalatestVersion = "3.2.11"

dependsOn(Projects.dataflowengineoss)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg"       % Versions.cpg,
  "org.soot-oss"   % "soot"              % sootVersion,
  "org.slf4j"      % "slf4j-api"         % slf4jVersion,
  "org.slf4j"      % "slf4j-simple"      % slf4jVersion,
  "org.scalatest" %% "scalatest"         % scalatestVersion % Test,
  "io.shiftleft"  %% "semanticcpg"       % Versions.cpg % Test classifier "tests",
)

enablePlugins(JavaAppPackaging)

trapExit := false
Test / fork := true
