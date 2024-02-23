name := "jimple2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"    %% "codepropertygraph" % Versions.cpg,
  "org.soot-oss"     % "soot"              % "4.4.1",
  "org.scalatest"   %% "scalatest"         % Versions.scalatest % Test,
  "org.benf"         % "cfr"               % "0.152",
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true
