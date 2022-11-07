name := "semanticcpg"

libraryDependencies ++= Seq(
  // TODO drop again, inherit from CPG
  "io.shiftleft"      %% "overflowdb-traversal" % "1.150+1-1e91bdba",

  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.json4s"    %% "json4s-native"     % Versions.json4s,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-g") //debug symbols
