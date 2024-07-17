name := "macros"

dependsOn(Projects.semanticcpg % Test)

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "net.oneandone.reflections8" % "reflections8" % "0.11.7",
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging)
