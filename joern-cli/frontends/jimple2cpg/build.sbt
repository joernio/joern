name := "jimple2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.soot-oss"   % "soot"              % Versions.soot,
  "org.typelevel" %% "cats-core"         % Versions.catsCore,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.benf"       % "cfr"               % Versions.cfr
)
//for heros 1.2.4 bug https://github.com/soot-oss/heros/commit/31f57b96a543de09092e7f351ed71115c80059ef
ThisBuild / dependencyOverrides ++= Seq(
  "com.google.guava" % "guava" % "33.5.0-jre"
)
enablePlugins(JavaAppPackaging, LauncherJarPlugin)

Test / testForkedParallel := false // soot is not thread-safe
