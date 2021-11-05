name := "javasrc2cpg"

scalaVersion := "2.13.6"

dependsOn(Projects.dataflowengineoss)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"  %% "semanticcpg"              % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.22.1",
  "org.scalatest" %% "scalatest"                % Versions.scalatest % Test
)


scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging)
trapExit := false
Global / onChangedBuildSource := ReloadOnSourceChanges
       	 
