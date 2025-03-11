lazy val V = _root_.scalafix.sbt.BuildInfo

lazy val scala3Version      = "3.5.2"

ThisBuild / name              := "project-linter"
ThisBuild / organization      := "io.joern"
ThisBuild / semanticdbEnabled := false

publish / skip := true

lazy val `project-linter` = (project in file("."))
  .aggregate(rules.projectRefs: _*)
  .settings(publish / skip := true)

lazy val rules = projectMatrix
  .settings(
    moduleName                            := "project-linter-rules",
    libraryDependencies += "ch.epfl.scala" % "scalafix-core_2.13" % V.scalafixVersion
  )
  .defaultAxes(VirtualAxis.jvm)
