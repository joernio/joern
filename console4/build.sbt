// use the manually created jar incl. it's deps
name := "console4"

enablePlugins(JavaAppPackaging)
scalaVersion := "3.2.0"

Compile/mainClass := Some("ammonite.AmmoniteMain")

libraryDependencies ++= Seq(
  "io.joern" %% "ammonite-fat" % "2.5.4-33-0af04a5b" cross CrossVersion.full,

  // TODO add these to the pom
  ("com.lihaoyi" %% "os-lib" % "0.8.1").cross(CrossVersion.for3Use2_13),
  ("com.lihaoyi" %% "upickle" % "2.0.0").cross(CrossVersion.for3Use2_13),
  ("com.lihaoyi" %% "ujson" % "2.0.0").cross(CrossVersion.for3Use2_13),
  "com.lihaoyi" %% "mainargs" % "0.3.0",
  "io.get-coursier" % "interface" % "0.0.21",
  "org.scala-lang" % "scala-reflect" % "2.13.9",
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0",

)
