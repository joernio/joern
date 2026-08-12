import sbt.BareBuildSyntax.dependsOn

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
  ("com.michaelpollmeier" % "scala-repl-pp-server" % Versions.scalaReplPP).cross(CrossVersion.full),
  "com.github.scopt"     %% "scopt"                % Versions.scopt,
  "org.typelevel"        %% "cats-effect"          % Versions.catsEffect,
  "org.zeroturnaround"    % "zt-zip"               % Versions.zeroTurnaround,
  "com.lihaoyi"          %% "os-lib"               % Versions.osLib,
  "com.lihaoyi"          %% "pprint"               % Versions.pPrint,
  "com.lihaoyi"          %% "cask"                 % Versions.cask,
  "org.apache.commons"    % "commons-lang3"        % Versions.commonsLang,
  "org.scalatest"        %% "scalatest"            % Versions.scalatest % Test
)

// Note: console tests invoke staged frontend binaries (c2cpg, jssrc2cpg, javasrc2cpg, swiftsrc2cpg
// under target/universal/stage). We deliberately do NOT wire `stage` into `Test / compile` here:
// staging pulls in `packagedArtifacts` of the whole dependency cone, whose sbt 2.x task cache
// content-hashes all test fixtures -- on Windows this fails when a forked test JVM still holds a
// file lock (https://github.com/sbt/sbt/issues/9816). Stage explicitly before running tests instead.
