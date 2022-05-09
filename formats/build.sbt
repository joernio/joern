name := "formats"

crossScalaVersions := Seq("2.13.8", "3.1.2")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
)

enablePlugins(JavaAppPackaging)
