name := "joern"
organization := "io.shiftleft"
scalaVersion := "2.12.7"

val cpgVersion = "0.9.101"

libraryDependencies ++= Seq(
  "io.shiftleft" % "codepropertygraph" % cpgVersion,
  "io.shiftleft" % "cpgloader-tinkergraph" % cpgVersion,
  "io.shiftleft" % "query-primitives" % cpgVersion,
  "io.shiftleft" % "enhancements" % cpgVersion,
)

enablePlugins(JavaAppPackaging)
