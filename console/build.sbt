name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.0.1"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.3"
val AmmoniteVersion       = "2.5.4-14-dc4c47bc"
val ZeroturnaroundVersion = "1.15"

dependsOn(
  Projects.semanticcpg,
  Projects.macros,
  Projects.jssrc2cpg,
  Projects.c2cpg % Test,
  Projects.x2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "org.scala-lang"       %% "scala3-compiler"   % scalaVersion.value, // for scala3 repl
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  "com.lihaoyi"          %% "os-lib"            % "0.8.1",
  "com.lihaoyi"          %% "pprint"            % "0.7.1",
  "com.lihaoyi"          %% "cask"              % CaskVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)
