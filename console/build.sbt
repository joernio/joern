name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.1.0"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.5"
val ZeroturnaroundVersion = "1.15"

dependsOn(
  Projects.semanticcpg,
  Projects.macros,
  Projects.javasrc2cpg,
  Projects.jssrc2cpg,
  Projects.pysrc2cpg,
  Projects.rubysrc2cpg,
  Projects.x2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "com.michaelpollmeier" %% "scala-repl-pp-all" % "0.1.32",
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  "com.lihaoyi"          %% "os-lib"            % "0.8.1",
  "com.lihaoyi"          %% "pprint"            % "0.7.3",
  "com.lihaoyi"          %% "cask"              % CaskVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)

Test/compile := (Test/compile).dependsOn((Projects.c2cpg/stage)).value
