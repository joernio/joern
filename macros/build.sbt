name := "macros"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
)

scalacOptions ++= Seq( "-Yrangepos" )
enablePlugins(JavaAppPackaging)

Test / packageBin / publishArtifact := true
