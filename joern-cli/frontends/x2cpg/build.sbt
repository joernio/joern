name := "x2cpg"

dependsOn(Projects.semanticcpg)

libraryDependencies ++= Seq(
  /* Start: AST Gen Dependencies */
  "com.lihaoyi"         %% "upickle"     % Versions.upickle,
  "com.typesafe"         % "config"      % Versions.typeSafeConfig,
  "com.michaelpollmeier" % "versionsort" % Versions.versionSort,
  /* End: AST Gen Dependencies */
  "org.gradle"     % "gradle-tooling-api" % Versions.gradleTooling % Optional,
  "org.scalatest" %% "scalatest"          % Versions.scalatest     % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := true

enablePlugins(JavaAppPackaging)

Universal / packageName       := name.value
Universal / topLevelDirectory := None
