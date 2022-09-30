// idea: play with ammonite and scala3: find a combination of jars that work together with scala3
// status: works - but would need to exclude fansi/sourcecode in every subproject downstream...
// idea: build a fat jar with everything 'ammonite*' and depend on that, plus selected transitive jars...
name := "console3"

enablePlugins(JavaAppPackaging)
scalaVersion := "3.2.0"

Compile/mainClass := Some("ammonite.AmmoniteMain")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "ammonite" % "2.5.4-33-0af04a5b" cross CrossVersion.full,
  // "com.lihaoyi" %% "fansi" % "0.3.1",
//   // "com.michaelpollmeier" %% "scala3-compiler"   % Scala3ReplForkVersion,
  // "org.scala-lang"       %% "scala3-compiler"   % scalaVersion.value,
  // "org.scala-lang"       %% "scala3-compiler"   % "3.1.3",
  // "org.ow2.asm" % "asm" % "9.3",
//  ("io.get-coursier"      %% "coursier"          % "2.0.13").cross(CrossVersion.for3Use2_13).exclude("org.scala-lang.modules", "scala-xml_2.13"),
//   "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
//   "com.github.scopt"     %% "scopt"             % ScoptVersion,
//   "org.typelevel"        %% "cats-effect"       % Versions.cats,
//   "io.circe"             %% "circe-generic"     % CirceVersion,
//   "io.circe"             %% "circe-parser"      % CirceVersion,
//   "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  // "com.lihaoyi"          %% "os-lib"            % "0.8.1",
//   "com.lihaoyi"          %% "pprint"            % "0.7.3",
//   "com.lihaoyi"          %% "cask"              % CaskVersion,
)

excludeDependencies ++= Seq(
  ExclusionRule("com.lihaoyi", "fansi_2.13"),
  ExclusionRule("com.lihaoyi", "sourcecode_2.13"),
)

scriptClasspath := Seq("*")
