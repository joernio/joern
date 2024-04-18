name := "console"

enablePlugins(JavaAppPackaging)

val CaskVersion           = "0.9.2"
val ZeroturnaroundVersion = "1.17"
val OsLibVersion          = "0.9.3"
val PprintVersion         = "0.8.1"
val CommonsLangVersion    = "3.14.0"

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
  "com.github.scopt"     %% "scopt"                % Versions.scopt,
  "org.typelevel"        %% "cats-effect"          % Versions.cats,
  "org.zeroturnaround"    % "zt-zip"               % ZeroturnaroundVersion,
  "com.lihaoyi"          %% "os-lib"               % OsLibVersion,
  "com.lihaoyi"          %% "pprint"               % PprintVersion,
  "com.lihaoyi"          %% "cask"                 % CaskVersion,
  "org.apache.commons"    % "commons-lang3"        % CommonsLangVersion,
  "org.scalatest"        %% "scalatest"            % Versions.scalatest % Test
)

Test / compile := (Test / compile)
  .dependsOn(Projects.c2cpg / stage, Projects.jssrc2cpg / stage, Projects.swiftsrc2cpg / stage)
  .value
