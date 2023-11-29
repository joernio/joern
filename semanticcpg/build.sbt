name := "semanticcpg"

val flatGraphVersion = "0.0.6"

libraryDependencies ++= Seq(
<<<<<<< HEAD
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier" %% "scala-repl-pp"     % Versions.scalaReplPP,
  "org.json4s"           %% "json4s-native"     % Versions.json4s,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
=======
  "io.joern" %% "joern-generated" % flatGraphVersion,
  "io.joern" %% "flatgraph-core" % flatGraphVersion,
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier" %% "scala-repl-pp" % Versions.scalaReplPP,
  "org.json4s"    %% "json4s-native"     % Versions.json4s,
  "org.scala-lang.modules" %% "scala-xml" % "2.2.0",
  "commons-lang" % "commons-lang" % "2.6",
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
>>>>>>> 5dd752fea (squashed: flatgraph port WIP)
)

Compile/compile/scalacOptions ++= Seq(
  // TODO remove
  "-Wconf:any:silent", // silence warnings for now
)
