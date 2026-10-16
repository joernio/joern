val kotlinVersion = "2.3.21"

lazy val kotlin2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.javasrc2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "kotlin2cpg",

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
    ),

    Test / javaOptions ++= Seq("-ea")
  )
