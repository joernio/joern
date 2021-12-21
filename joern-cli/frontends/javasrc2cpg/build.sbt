name := "javasrc2cpg"

scalaVersion := "2.13.7"

dependsOn(Projects.dataflowengineoss)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg"              % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % Versions.log4j % Runtime,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.23.1",
  "org.scalatest" %% "scalatest"                % Versions.scalatest % Test
)


scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging)
trapExit := false
Global / onChangedBuildSource := ReloadOnSourceChanges
       	 
