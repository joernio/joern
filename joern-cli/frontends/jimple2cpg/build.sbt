lazy val jimple2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "jimple2cpg",

    libraryDependencies ++= Seq(
      "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
      "org.soot-oss"   % "soot"              % Versions.soot,
      "org.typelevel" %% "cats-core"         % Versions.catsCore,
      "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
      "org.benf"       % "cfr"               % Versions.cfr
    ),

    Test / testForkedParallel := false // soot is not thread-safe
  )
