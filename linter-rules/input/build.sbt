name := "linter-rules-input"
// Inherits ThisBuild scalaVersion so the SemanticDB the testkit exercises
// matches what the rule sees in CI/production runs.
// semanticdbEnabled (ThisBuild) makes the Scala 3 compiler emit SemanticDB via -Xsemanticdb.
disablePlugins(ScalafixPlugin)
scalacOptions := Seq("-deprecation", "-feature")
// testkit input fixtures only — never released
publish / skip := true
