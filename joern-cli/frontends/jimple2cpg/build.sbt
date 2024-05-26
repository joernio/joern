name := "jimple2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.soot-oss"   % "soot"              % Versions.soot,
  "org.typelevel" %% "cats-core"         % Versions.catsCore,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test,
  "org.benf"       % "cfr"               % Versions.cfr
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true

githubOwner      := "Privado-Inc"
githubRepository := "joern"
credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
