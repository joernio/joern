name := "slicing"

crossScalaVersions := Seq("2.13.8", "3.2.2")

dependsOn(Projects.semanticcpg, Projects.dataflowengineoss)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.14.5",
  "io.circe" %% "circe-generic" % "0.14.5",
)