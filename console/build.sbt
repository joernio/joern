name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.1.0"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.2"
// val AmmoniteVersion       = "2.5.4+4-fatter-mainargs030"
val ZeroturnaroundVersion = "1.15"

dependsOn(Projects.semanticcpg, Projects.macros, Projects.c2cpg % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  // "io.joern"             %% "ammonite-fat"      % AmmoniteVersion cross CrossVersion.full,
  "com.michaelpollmeier.ammolite" %% "ammolite" % "2.5.4-ammolite+0-42618e9b+20221007-0906",
  "com.lihaoyi"          %% "cask"              % CaskVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)

Test / packageBin / publishArtifact := true

// would love to reenable, but somehow StorageBackend.scala triggers a strange `[warn] method with a single empty parameter list overrides method without any parameter list` that doesn't make sense to me...
scalacOptions -= "-Xfatal-warnings"
