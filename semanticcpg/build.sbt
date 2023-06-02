name := "semanticcpg"

crossScalaVersions := Seq("2.13.8", "3.3.0")

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "org.json4s"    %% "json4s-native"     % Versions.json4s,
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-g") //debug symbols
