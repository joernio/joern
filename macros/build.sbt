name := "macros"

dependsOn(Projects.console)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq( "-Yrangepos" )
enablePlugins(JavaAppPackaging)

Test / packageBin / publishArtifact := true
