name := "linter-rules"
//scalafix otherwise gets unhappy because of circular dependency
disablePlugins(ScalafixPlugin)

// scalafix-core is not published for Scala 3 (scalacenter/scalafix#2041); the 2.13 artifact
// is the canonical rule API even for rules compiled with Scala 3 (CrossVersion.for3Use2_13).
libraryDependencies += ("ch.epfl.scala" %% "scalafix-core" % _root_.scalafix.sbt.BuildInfo.scalafixVersion)
  .cross(CrossVersion.for3Use2_13)

// The testkit is published per full Scala version, but only for the Scala 3 version current
// at the scalafix release date (0.14.7 ships 3.3.8 and 3.8.4 — no 3.8.3 exists), so an exact
// match with ThisBuild scalaVersion is generally unavailable. A patch mismatch is harmless:
// patches within a Scala 3 minor are binary/TASTy-compatible both ways, and the SemanticDB
// under test is produced by the input project's own compiler (3.8.3), not by the testkit
// artifact — the testkit only provides the harness classes.
// TODO: Revisit on a new Scala 3 minor.
libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_3.8.4" % _root_.scalafix.sbt.BuildInfo.scalafixVersion % Test

Test / resourceGenerators += Def.task {
  val inputClasspath =
    (LocalProject("linterRulesInput") / Compile / fullClasspath).value
  val inputSourceDirs =
    (LocalProject("linterRulesInput") / Compile / unmanagedSourceDirectories).value
  val sourceroot = (ThisBuild / baseDirectory).value
  val scalaVer =
    (LocalProject("linterRulesInput") / scalaVersion).value
  val scalacOpts =
    (LocalProject("linterRulesInput") / Compile / scalacOptions).value

  val props = new java.util.Properties()
  def putFiles(key: String, files: Seq[java.io.File]): Unit = {
    props.put(
      key,
      files.iterator.filter(_.exists()).mkString(java.io.File.pathSeparator)
    )
  }

  putFiles("inputClasspath", inputClasspath.map(_.data))
  putFiles("inputSourceDirectories", inputSourceDirs)
  putFiles("outputSourceDirectories", Seq.empty[java.io.File])
  putFiles("sourceroot", Seq(sourceroot))
  props.put("scalaVersion", scalaVer)
  props.put("scalacOptions", scalacOpts.mkString("|"))

  val outputFile =
    (Test / managedResourceDirectories).value.head / "scalafix-testkit.properties"
  IO.write(props, "Input data for scalafix testkit", outputFile)
  Seq(outputFile)
}
