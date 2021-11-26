name := "macros"

enablePlugins(JavaAppPackaging)

dependsOn(Projects.codepropertygraph)
scalacOptions ++= Seq( "-Yrangepos" )

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
)

Test / packageBin / publishArtifact := true
