lazy val x2cpg = (project in file("."))
  .dependsOn(
    Projects.semanticcpg,
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "x2cpg",

    libraryDependencies ++= Seq(
      /* Start: AST Gen Dependencies */
      "com.lihaoyi"         %% "upickle"      % Versions.upickle,
      "com.typesafe"         % "config"       % Versions.typeSafeConfig,
      "com.michaelpollmeier" % "versionsort"  % Versions.versionSort,
      "org.apache.commons"   % "commons-exec" % Versions.commonsExec,
      /* End: AST Gen Dependencies */
      "org.apache.maven" % "maven-artifact"     % Versions.mavenArtifact,
      "org.gradle"     % "gradle-tooling-api" % Versions.gradleTooling % Optional,
      "org.scalatest" %% "scalatest"          % Versions.scalatest     % Test
    ),

    Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value),

    compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g"),
    Test / scalacOptions += "-language:implicitConversions",

    Universal / packageName       := name.value,
    Universal / topLevelDirectory := None
  )
