name := "dataflowengineoss"

dependsOn(Projects.semanticcpg, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "org.antlr"               % "antlr4"                     % Versions.antlr,
  "org.antlr"               % "antlr4-runtime"             % Versions.antlr,
  "com.lihaoyi"            %% "upickle"                    % Versions.upickle,
  "com.lihaoyi"            %% "ujson"                      % Versions.upickle,
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test,
  "org.scala-lang.modules" %% "scala-parallel-collections" % Versions.scalaParallel
)

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("io.joern.dataflowengineoss")
Antlr4 / antlr4Version     := Versions.antlr
Antlr4 / javaSource        := (Compile / sourceManaged).value
Compile / doc / sources ~= (_ filter (_ => false))

githubOwner      := "Privado-Inc"
githubRepository := "joern"
credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
