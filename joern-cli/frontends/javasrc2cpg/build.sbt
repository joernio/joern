name := "javasrc2cpg"

scalaVersion := "2.13.6"

trapExit := false

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg"              % Versions.cpg,
  "io.shiftleft"  %% "dataflowengineoss"        % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.22.1",
  "org.scalatest" %% "scalatest"                % Versions.scalatest % Test
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
       	 
