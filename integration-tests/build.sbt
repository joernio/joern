// End-to-end tests that drive the real console / launcher scripts. They need staged frontends, so they are
// intentionally kept out of the root aggregate (and thus out of `test`, `testFull` and any publishing).
// Run them via: sbt "c2cpg/stage ; integrationTests/test"

lazy val integrationTests = (project in file("."))
  .dependsOn(
    Projects.joerncli,
    Projects.linterRules % ScalafixConfig
  )
  .settings(
    name := "integration-tests",

    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % Versions.scalatest % Test),

    publish / skip := true,

    // scripts are compiled in-process with the inherited classpath, which requires running in the sbt JVM
    Test / fork := false
  )
