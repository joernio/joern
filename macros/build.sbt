name := "macros"

dependsOn(Projects.semanticcpg % Test)

libraryDependencies ++= Seq(
  "com.michaelpollmeier"  %% "codepropertygraph" % Versions.cpg,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging)
