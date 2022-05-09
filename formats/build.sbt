name := "formats"

crossScalaVersions := Seq("2.13.8", "3.1.2")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % Versions.log4j % Optional,
)

enablePlugins(JavaAppPackaging)
