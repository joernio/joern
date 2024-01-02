name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.1.0"
val CaskVersion           = "0.9.1"
val CirceVersion          = "0.14.6"
val ZeroturnaroundVersion = "1.15"
val OsLibVersion          = "0.9.1"
val PprintVersion         = "0.7.3"
val CommonsLangVersion    = "3.12.0"

dependsOn(
  Projects.semanticcpg,
  Projects.macros,
  Projects.javasrc2cpg,
  Projects.jssrc2cpg,
  Projects.php2cpg,
  Projects.pysrc2cpg,
  Projects.rubysrc2cpg,
  Projects.swiftsrc2cpg,
  Projects.x2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "io.shiftleft"         %% "codepropertygraph"    % Versions.cpg,
  "com.michaelpollmeier" %% "scala-repl-pp-server" % Versions.scalaReplPP,
  "com.github.scopt"     %% "scopt"                % ScoptVersion,
  "org.typelevel"        %% "cats-effect"          % Versions.cats,
  "io.circe"             %% "circe-generic"        % CirceVersion,
  "io.circe"             %% "circe-parser"         % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"               % ZeroturnaroundVersion,
  "com.lihaoyi"          %% "os-lib"               % OsLibVersion,
  "com.lihaoyi"          %% "pprint"               % PprintVersion,
  "com.lihaoyi"          %% "cask"                 % CaskVersion,
  "org.apache.commons"    % "commons-lang3"        % CommonsLangVersion,
  "org.scalatest"        %% "scalatest"            % Versions.scalatest % Test
)

Test / compile := (Test / compile).dependsOn((Projects.c2cpg / stage)).value
