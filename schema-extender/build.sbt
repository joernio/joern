name := "schema-extender"
publish / skip := true

// the overflowdb-codegen is only available for scala 2.12 (driven by sbt being on 2.12), so we'll just do the same here
scalaVersion := "2.12.11"
libraryDependencies += "io.shiftleft" %% "schema-extender" % Versions.cpgVersion

enablePlugins(JavaAppPackaging)
Compile/mainClass := Some("io.shiftleft.repl.schemaextender.SchemaExtender")

