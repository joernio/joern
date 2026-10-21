lazy val macros = (project in file("."))
  .dependsOn(
    Projects.semanticcpg % Test,
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "macros",

    libraryDependencies ++= Seq(
      "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
      "net.oneandone.reflections8" % "reflections8"      % "0.11.7",
      "com.lihaoyi"               %% "upickle"           % Versions.upickle,
      "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test
    )
  )
