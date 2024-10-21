name := "kotlin2cpg"

val kotlinVersion = "1.9.23"

dependsOn(
  Projects.dataflowengineoss % "compile->compile;test->test",
  Projects.x2cpg       % "compile->compile;test->test",
  Projects.javasrc2cpg % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "com.lihaoyi"             %% "requests"                   % Versions.requests,
  "com.lihaoyi"             %% "ujson"                      % Versions.upickle,
  "com.squareup.tools.build" % "maven-archeologist"         % Versions.mavenArcheologist,
  "io.shiftleft"            %% "codepropertygraph"          % Versions.cpg,
  "org.gradle"               % "gradle-tooling-api"         % Versions.gradleTooling,
  "org.jetbrains.kotlin"     % "kotlin-stdlib-jdk8"         % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-stdlib"              % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-compiler-embeddable" % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-allopen"             % kotlinVersion,
  "org.scalatest"           %% "scalatest"                  % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true
Test / javaOptions ++= Seq("-ea")
