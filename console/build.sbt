name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.0.1"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.2"
val ZeroturnaroundVersion = "1.15"
val Scala3ReplForkVersion = "3.1.3-bin-20220912-0999a7a-NIGHTLY-nonbootstrapped"

dependsOn(Projects.semanticcpg, Projects.macros, Projects.c2cpg % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  // TODO once https://github.com/lampepfl/dotty/pull/16011 is merged and released as part of 3.2.1, use our custom scala fork here 
  "com.michaelpollmeier" %% "scala3-compiler"   % Scala3ReplForkVersion,
 ("io.get-coursier"      %% "coursier"          % "2.0.13").cross(CrossVersion.for3Use2_13).exclude("org.scala-lang.modules", "scala-xml_2.13"),
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

Test / packageBin / publishArtifact := true
