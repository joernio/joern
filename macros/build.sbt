name := "macros"

dependsOn(Projects.semanticcpg % Test)

val flatGraphVersion = "0.0.5+4-27b095f7+20231128-0842"

libraryDependencies ++= Seq(
  // TODO drop
  "io.joern" %% "flatgraph-core" % flatGraphVersion,
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging)
