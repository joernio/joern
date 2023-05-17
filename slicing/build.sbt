name := "slicing"

crossScalaVersions := Seq("2.13.8", "3.2.2")

dependsOn(Projects.semanticcpg, Projects.dataflowengineoss)

lazy val circeVersion = "0.15.0-M1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)