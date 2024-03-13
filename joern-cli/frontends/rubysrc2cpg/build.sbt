name := "rubysrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.apache.commons" % "commons-compress" % "1.26.1", // For unpacking Gems with `--download-dependencies`
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.antlr"      % "antlr4-runtime"    % Versions.antlr
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin, Antlr4Plugin)

Antlr4 / antlr4Version    := Versions.antlr
Antlr4 / antlr4GenVisitor := true
Antlr4 / javaSource       := (Compile / sourceManaged).value
