name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.0.1"
val BetterFilesVersion    = "3.9.1"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.2"
val AmmoniteVersion       = "2.5.4+3-fatterjar"
val ZeroturnaroundVersion = "1.15"

dependsOn(Projects.semanticcpg, Projects.macros, Projects.c2cpg % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.joern" %% "ammonite-fat" % AmmoniteVersion cross CrossVersion.full,
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi" %% "cask" % CaskVersion,
  "com.github.pathikrit" %% "better-files"      % BetterFilesVersion cross CrossVersion.for3Use2_13,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test,
)

Test / packageBin / publishArtifact := true

// would love to reenable, but somehow StorageBackend.scala triggers a strange `[warn] method with a single empty parameter list overrides method without any parameter list` that doesn't make sense to me...
scalacOptions -= "-Xfatal-warnings"
