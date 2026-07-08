name := "lua2cpg"

dependsOn(
  Projects.x2cpg        % "compile->compile;test->test",
  Projects.linterRules  % ScalafixConfig
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
