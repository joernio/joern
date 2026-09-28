name := "macros"

dependsOn(Projects.semanticcpg % Test, Projects.linterRules % ScalafixConfig)

libraryDependencies ++= Seq(
  "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
  "net.oneandone.reflections8" % "reflections8"      % "0.11.7",
  "com.lihaoyi"               %% "upickle"           % Versions.upickle,
  "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging)
