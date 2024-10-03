name := "semanticcpg"

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier"   %% "scala-repl-pp"     % Versions.scalaReplPP,
  "org.json4s"             %% "json4s-native"     % Versions.json4s,
  "org.scala-lang.modules" %% "scala-xml"         % "2.2.0",
  "org.apache.commons"      % "commons-text"      % Versions.commonsText,
  "org.scalatest"          %% "scalatest"         % Versions.scalatest % Test,
  "com.amazon.ion"          % "ion-java"          % "1.11.9",
  "com.amazon.ion"          % "ion-schema-kotlin" % "1.8.0",
  "com.github.zafarkhaja"   % "java-semver"       % "0.10.2"
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

enablePlugins(JmhPlugin)
