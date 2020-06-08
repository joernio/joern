name := "schema-extender"
publish / skip := true

// the overflowdb-codegen is only available for scala 2.12 (driven by sbt being on 2.12), so we'll just do the same here
scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "overflowdb-codegen" % "1.12",
  "org.zeroturnaround" % "zt-zip" % "1.14",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.github.scopt" %% "scopt" % "3.7.1",
)

enablePlugins(JavaAppPackaging)
Compile/mainClass := Some("io.shiftleft.repl.schemaextender.SchemaExtender")

