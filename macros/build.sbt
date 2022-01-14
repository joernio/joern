name := "macros"

scalaVersion := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.1.0")
libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg % Test,
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
    ) ++ (
      CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq()
      case _ => Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
      }
      )

scalacOptions ++= Seq( ) ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq()
    case _ => Seq(
        "-Yrangepos", 
        )   
    }
    )

enablePlugins(JavaAppPackaging)

Test / packageBin / publishArtifact := true
