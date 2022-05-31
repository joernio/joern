name := "kotlin2cpg"

val kotlinVersion = "1.6.21"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.github.pathikrit"    %% "better-files"               % "3.9.1",
  "io.shiftleft"            %% "codepropertygraph"          % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl"           % Versions.log4j         % Runtime,
  "org.slf4j"                % "slf4j-api"                  % "1.7.35",
  "org.gradle"               % "gradle-tooling-api"         % Versions.gradleTooling % Optional,
  "org.jetbrains.kotlin"     % "kotlin-stdlib-jdk8"         % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-stdlib"              % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-compiler-embeddable" % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-allopen"             % kotlinVersion,
  "org.jetbrains.kotlin"     % "kotlin-test"                % kotlinVersion          % Test,
  "org.scalatest"           %% "scalatest"                  % "3.2.9"                % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit := false
Test/fork := false
