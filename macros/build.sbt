name := "macros"

enablePlugins(JavaAppPackaging)

scalacOptions ++= Seq( "-Yrangepos" )

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
)

Test / packageBin / publishArtifact := true
