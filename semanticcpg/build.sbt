name := "semanticcpg"

dependsOn(Projects.linterRules % ScalafixConfig)

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier"    % "scala-repl-pp"     % Versions.scalaReplPP cross CrossVersion.full,
  "org.json4s"             %% "json4s-native"     % Versions.json4s,
  "org.scala-lang.modules" %% "scala-xml"         % "2.2.0",
  "org.apache.commons"      % "commons-text"      % Versions.commonsText,
  "org.scalatest"          %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)
