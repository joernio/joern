lazy val javasrc2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "javasrc2cpg",

    libraryDependencies ++= Seq(
      "io.shiftleft"           %% "codepropertygraph"             % Versions.cpg,
      "com.github.javaparser"   % "javaparser-symbol-solver-core" % Versions.javaParser,
      "org.gradle"              % "gradle-tooling-api"            % Versions.gradleTooling,
      "org.scalatest"          %% "scalatest"                     % Versions.scalatest % Test,
      "org.projectlombok"       % "lombok"                        % Versions.lombok,
      "org.ow2.asm"             % "asm"                           % Versions.asm
    )
  )
