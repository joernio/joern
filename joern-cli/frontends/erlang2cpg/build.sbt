lazy val erlang2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "erlang2cpg",

    libraryDependencies ++= Seq(
      "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
      "com.lihaoyi"   %% "ujson"             % Versions.upickle,
      "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
    )
  )
