name := "linter-rules"
//scalafix otherwise gets unhappy because of circular dependency
disablePlugins(ScalafixPlugin)

libraryDependencies +=
  "ch.epfl.scala" % "scalafix-core_2.13" % _root_.scalafix.sbt.BuildInfo.scalafixVersion
