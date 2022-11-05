name := "kotlin2cpg"

val kotlinVersion = "1.6.21"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test", Projects.javasrc2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.lihaoyi"             %% "requests"                   % "0.7.0",
  "com.lihaoyi"             %% "ujson"                      % "2.0.0",
  "com.squareup.tools.build" % "maven-archeologist"         % "0.0.10",
  "io.shiftleft"            %% "codepropertygraph"          % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl"           % Versions.log4j % Runtime,
  "org.slf4j"                % "slf4j-api"                  % "1.7.35",
  "org.gradle"               % "gradle-tooling-api"         % Versions.gradleTooling,
  "org.jetbrains.kotlin"     % "kotlin-stdlib-jdk8"         % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-stdlib"              % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-compiler-embeddable" % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-allopen"             % kotlinVersion,
  "org.scalatest"           %% "scalatest"                  % "3.2.9"        % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := false
