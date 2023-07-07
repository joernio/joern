name := "rubysrc2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.antlr"      % "antlr4-runtime"    % Versions.antlr,
  "org.jruby"      % "jruby-base"        % Versions.jruby
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin, Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("io.joern.rubysrc2cpg.parser")
Antlr4 / antlr4Version     := Versions.antlr
Antlr4 / antlr4GenVisitor  := true
Antlr4 / javaSource        := (Compile / sourceManaged).value
