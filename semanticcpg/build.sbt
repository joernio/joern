name := "semanticcpg"

libraryDependencies ++= Seq(
  "com.michaelpollmeier"           %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier"   %% "scala-repl-pp"     % Versions.scalaReplPP,
  "org.json4s"             %% "json4s-native"     % Versions.json4s,
  "org.scala-lang.modules" %% "scala-xml"         % "2.2.0",
  "org.apache.commons"      % "commons-text"      % Versions.commonsText,
  "org.scalatest"          %% "scalatest"         % Versions.scalatest % Test
)

Compile/compile/scalacOptions ++= Seq(
  // TODO remove
  "-Wconf:any:silent", // silence warnings for now
)
