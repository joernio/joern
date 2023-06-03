name := "dataflowengineoss"

crossScalaVersions := Seq("2.13.8", "3.3.0")

dependsOn(Projects.semanticcpg, Projects.x2cpg)

libraryDependencies ++= Seq(
  "org.antlr"               % "antlr4-runtime"             % Versions.antlr,
  "io.circe"               %% "circe-core"                 % Versions.circe,
  "io.circe"               %% "circe-generic"              % Versions.circe,
  "io.circe"               %% "circe-parser"               % Versions.circe,
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("io.joern.dataflowengineoss")
Antlr4 / antlr4Version     := Versions.antlr
Antlr4 / javaSource        := (Compile / sourceManaged).value
Compile / doc / sources ~= (_ filter (_ => false))
