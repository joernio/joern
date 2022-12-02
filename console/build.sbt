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
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  // "com.lihaoyi"          %% "ammonite"          % AmmoniteVersion cross CrossVersion.full,
  "com.lihaoyi"          %  "ammonite_3.1.3"    % AmmoniteVersion,
  "com.lihaoyi"          %% "os-lib"            % "0.8.1",
  "com.lihaoyi"          %% "cask"              % CaskVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)

// would love to reenable, but somehow StorageBackend.scala triggers a strange `[warn] method with a single empty parameter list overrides method without any parameter list` that doesn't make sense to me...
scalacOptions -= "-Xfatal-warnings"
