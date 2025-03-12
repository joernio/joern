import sbt.*
import sbt.Keys.*
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.ScalafixConfig

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  // lazy val joerncli          = project.in(file("joern-cli"))
  lazy val joerncli          = commonSettings(project.in(file("joern-cli")))
  lazy val querydb           = project.in(file("querydb"))
  lazy val console           = commonSettings(project.in(file("console")))
  lazy val dataflowengineoss = project.in(file("dataflowengineoss"))
  lazy val macros            = project.in(file("macros"))
  lazy val semanticcpg       = project.in(file("semanticcpg"))

  // lazy val c2cpg         = project.in(frontendsRoot / "c2cpg")
  lazy val c2cpg         = commonSettings(project.in(frontendsRoot / "c2cpg"))
  lazy val ghidra2cpg    = project.in(frontendsRoot / "ghidra2cpg")
  lazy val x2cpg         = project.in(frontendsRoot / "x2cpg")
  lazy val pysrc2cpg     = project.in(frontendsRoot / "pysrc2cpg")
  // lazy val php2cpg       = project.in(frontendsRoot / "php2cpg")
  lazy val php2cpg       = commonSettings(project.in(frontendsRoot / "php2cpg"))
  lazy val jssrc2cpg     = project.in(frontendsRoot / "jssrc2cpg")
  lazy val swiftsrc2cpg  = project.in(frontendsRoot / "swiftsrc2cpg")
  lazy val javasrc2cpg   = project.in(frontendsRoot / "javasrc2cpg")
  lazy val jimple2cpg    = project.in(frontendsRoot / "jimple2cpg")
  lazy val kotlin2cpg    = project.in(frontendsRoot / "kotlin2cpg")
  lazy val rubysrc2cpg   = project.in(frontendsRoot / "rubysrc2cpg")
  lazy val gosrc2cpg     = project.in(frontendsRoot / "gosrc2cpg")
  lazy val csharpsrc2cpg = project.in(frontendsRoot / "csharpsrc2cpg")

  // lazy val joerncli          = newProject("joern-cli")
  // lazy val querydb           = newProject("querydb")
  // lazy val console           = newProject("console")
  // lazy val dataflowengineoss = newProject("dataflowengineoss")
  // lazy val macros            = newProject("macros")
  // lazy val semanticcpg       = newProject("semanticcpg")

  // lazy val c2cpg         = newFrontendProject("c2cpg")
  // lazy val ghidra2cpg    = newFrontendProject("ghidra2cpg")
  // lazy val x2cpg         = newFrontendProject("x2cpg")
  // lazy val pysrc2cpg     = newFrontendProject("pysrc2cpg")
  // lazy val php2cpg       = newFrontendProject("php2cpg")
  // lazy val jssrc2cpg     = newFrontendProject("jssrc2cpg")
  // lazy val swiftsrc2cpg  = newFrontendProject("swiftsrc2cpg")
  // lazy val javasrc2cpg   = newFrontendProject("javasrc2cpg")
  // lazy val jimple2cpg    = newFrontendProject("jimple2cpg")
  // lazy val kotlin2cpg    = newFrontendProject("kotlin2cpg")
  // lazy val rubysrc2cpg   = newFrontendProject("rubysrc2cpg")
  // lazy val gosrc2cpg     = newFrontendProject("gosrc2cpg")
  // lazy val csharpsrc2cpg = newFrontendProject("csharpsrc2cpg")

  // lazy val projectLinter = project.in(file("project-linter"))
  lazy val projectLinterRules = project.in(file("linter-rules"))
    // .settings(libraryDependencies +=
    //   "ch.epfl.scala" % "scalafix-core_2.13" % _root_.scalafix.sbt.BuildInfo.scalafixVersion
    // )

  def commonSettings(proj: Project): Project = {
    println(s"XXX0 before: $proj")
    val ret = proj.dependsOn(projectLinterRules % ScalafixConfig)
    println(s"XXX0 after: $ret")
    ret
  }

  // private def newProject(name: String) = 
  //   Project(name, file(name))
      // .dependsOn(projectLinterRules % ScalafixConfig)

  // lazy val frontendsRoot = file("joern-cli/frontends")
  // private def newFrontendProject(name: String) = 
  //   Project(name, frontendsRoot / name)
      // .dependsOn(projectLinterRules % ScalafixConfig)
      // .dependsOn(projectLinterRules)
      // .settings(
      //   projectDependencies += projectLinterRules
      // )
}
