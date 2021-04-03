name := "schema-extender"
publish / skip := true

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph-schema" % Versions.cpgVersion,
  "org.zeroturnaround" % "zt-zip" % "1.14",
  "com.github.scopt" %% "scopt" % "4.0.1",
)

enablePlugins(JavaAppPackaging)
Compile/mainClass := Some("io.shiftleft.repl.schemaextender.SchemaExtender")

