import sbt.*
import sbt.Keys.*
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.ScalafixConfig

object Projects {

  lazy val joerncli          = newProject("joern-cli")
  lazy val querydb           = newProject("querydb")
  lazy val console           = newProject("console")
  lazy val dataflowengineoss = newProject("dataflowengineoss")
  lazy val macros            = newProject("macros")
  lazy val semanticcpg       = newProject("semanticcpg")

  lazy val c2cpg         = newFrontendProject("c2cpg")
  lazy val ghidra2cpg    = newFrontendProject("ghidra2cpg")
  lazy val x2cpg         = newFrontendProject("x2cpg")
  lazy val pysrc2cpg     = newFrontendProject("pysrc2cpg")
  // lazy val php2cpg       = newFrontendProject("php2cpg")
  lazy val php2cpg       = Project("php2cpg", frontendsRoot / "php2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val jssrc2cpg     = newFrontendProject("jssrc2cpg")
  lazy val swiftsrc2cpg  = newFrontendProject("swiftsrc2cpg")
  lazy val javasrc2cpg   = newFrontendProject("javasrc2cpg")
  lazy val jimple2cpg    = newFrontendProject("jimple2cpg")
  lazy val kotlin2cpg    = newFrontendProject("kotlin2cpg")
  lazy val rubysrc2cpg   = newFrontendProject("rubysrc2cpg")
  lazy val gosrc2cpg     = newFrontendProject("gosrc2cpg")
  lazy val csharpsrc2cpg = newFrontendProject("csharpsrc2cpg")

  lazy val projectLinter = project.in(file("project-linter"))
  lazy val projectLinterRules = (project in file("project-linter/rules"))
    .disablePlugins(ScalafixPlugin)
    .settings(
      libraryDependencies +=
        "ch.epfl.scala" % "scalafix-core_2.13" % _root_.scalafix.sbt.BuildInfo.scalafixVersion
    )

  private def newProject(name: String) = 
    Project(name, file(name))
      .dependsOn(projectLinterRules % ScalafixConfig)

  lazy val frontendsRoot = file("joern-cli/frontends")
  private def newFrontendProject(name: String) = 
    Project(name, frontendsRoot / name)
      .dependsOn(projectLinterRules % ScalafixConfig)
}
