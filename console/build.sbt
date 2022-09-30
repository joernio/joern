name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.0.1"
val BetterFilesVersion    = "3.9.1"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.2"
val AmmoniteVersion       = "2.5.4+1-5d34cfc3"
val ZeroturnaroundVersion = "1.15"

dependsOn(Projects.semanticcpg, Projects.macros, Projects.c2cpg % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.joern" %% "ammonite-fat" % AmmoniteVersion cross CrossVersion.full,
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.pathikrit" %% "better-files"      % BetterFilesVersion cross CrossVersion.for3Use2_13,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test,

  // "com.lihaoyi"          %% "cask"              % CaskVersion cross CrossVersion.for3Use2_13, //leads to more `conflicting cross-version suffixes` errors...
  "com.lihaoyi" % "cask_2.13" % CaskVersion,
)

Test / packageBin / publishArtifact := true
