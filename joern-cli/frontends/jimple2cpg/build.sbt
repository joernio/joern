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
dependencyOverrides += "com.google.guava" % "guava" % "33.5.0-jre" // required currently because of the dependencies of soot 4.7.1

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true
