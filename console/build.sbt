name := "console"

enablePlugins(JavaAppPackaging)

dependsOn(
  Projects.semanticcpg,
  Projects.macros,
  Projects.rubysrc2cpg,
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"         %% "codepropertygraph"    % Versions.cpg,
  "com.michaelpollmeier"  % "scala-repl-pp-server" % Versions.scalaReplPP cross CrossVersion.full,
  "com.github.scopt"     %% "scopt"                % Versions.scopt,
  "org.typelevel"        %% "cats-effect"          % Versions.catsEffect,
  "org.zeroturnaround"    % "zt-zip"               % Versions.zeroTurnaround,
  "com.lihaoyi"          %% "os-lib"               % Versions.osLib,
  "com.lihaoyi"          %% "pprint"               % Versions.pPrint,
  "com.lihaoyi"          %% "cask"                 % Versions.cask,
  "org.apache.commons"    % "commons-lang3"        % Versions.commonsLang,
  "org.scalatest"        %% "scalatest"            % Versions.scalatest % Test
)

Test / compile := (Test / compile)
  .dependsOn(Projects.c2cpg / stage, Projects.jssrc2cpg / stage, Projects.swiftsrc2cpg / stage)
  .value
