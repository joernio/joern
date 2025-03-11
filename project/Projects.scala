import sbt.*
import sbt.Keys.*
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.ScalafixConfig

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  lazy val joerncli          = project.in(file("joern-cli")).dependsOn(projectLinterRules % ScalafixConfig)
  lazy val querydb           = project.in(file("querydb")).dependsOn(projectLinterRules % ScalafixConfig)
  lazy val console           = project.in(file("console")).dependsOn(projectLinterRules % ScalafixConfig)
  lazy val dataflowengineoss = project.in(file("dataflowengineoss")).dependsOn(projectLinterRules % ScalafixConfig)
  lazy val macros            = project.in(file("macros")).dependsOn(projectLinterRules % ScalafixConfig)
  lazy val semanticcpg       = project.in(file("semanticcpg")).dependsOn(projectLinterRules % ScalafixConfig)

  lazy val c2cpg         = project.in(frontendsRoot / "c2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val ghidra2cpg    = project.in(frontendsRoot / "ghidra2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val x2cpg         = project.in(frontendsRoot / "x2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val pysrc2cpg     = project.in(frontendsRoot / "pysrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val php2cpg       = project.in(frontendsRoot / "php2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val jssrc2cpg     = project.in(frontendsRoot / "jssrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val swiftsrc2cpg  = project.in(frontendsRoot / "swiftsrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val javasrc2cpg   = project.in(frontendsRoot / "javasrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val jimple2cpg    = project.in(frontendsRoot / "jimple2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val kotlin2cpg    = project.in(frontendsRoot / "kotlin2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val rubysrc2cpg   = project.in(frontendsRoot / "rubysrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val gosrc2cpg     = project.in(frontendsRoot / "gosrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)
  lazy val csharpsrc2cpg = project.in(frontendsRoot / "csharpsrc2cpg").dependsOn(projectLinterRules % ScalafixConfig)

  lazy val projectLinter = project.in(file("project-linter"))
  lazy val projectLinterRoot = file("project-linter")
  lazy val projectLinterRules = (project in projectLinterRoot / "rules")
    .disablePlugins(ScalafixPlugin)
    .settings(
      libraryDependencies +=
        "ch.epfl.scala" % "scalafix-core_2.13" % _root_.scalafix.sbt.BuildInfo.scalafixVersion
    )



}
