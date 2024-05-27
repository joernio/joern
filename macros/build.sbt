name := "macros"

dependsOn(Projects.semanticcpg % Test)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging)

githubOwner      := "Privado-Inc"
githubRepository := "joern"
credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
