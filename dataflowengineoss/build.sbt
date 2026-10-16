lazy val dataflowengineoss = (project in file("."))
  .dependsOn(
    Projects.semanticcpg,
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "dataflowengineoss",

    libraryDependencies ++= Seq(
      "org.antlr"               % "antlr4"                     % Versions.antlr,
      "org.antlr"               % "antlr4-runtime"             % Versions.antlr,
      "com.lihaoyi"            %% "upickle"                    % Versions.upickle,
      "com.lihaoyi"            %% "ujson"                      % Versions.upickle,
      "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test
    ),

    Antlr4 / antlr4Package := Some("io.joern.dataflowengineoss"),
    Antlr4 / antlr4Version := Some(Versions.antlr),
    Compile / doc / sources ~= (_ filter (_ => false))
  )
