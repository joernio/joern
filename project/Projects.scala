import sbt.*

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  lazy val joerncli          = project.in(file("joern-cli"))
  lazy val querydb           = project.in(file("querydb"))
  lazy val console           = project.in(file("console"))
  lazy val dataflowengineoss = project.in(file("dataflowengineoss"))
  lazy val macros            = project.in(file("macros"))
  lazy val semanticcpg       = project.in(file("semanticcpg"))

  lazy val c2cpg         = project.in(frontendsRoot / "c2cpg")
  lazy val ghidra2cpg    = project.in(frontendsRoot / "ghidra2cpg")
  lazy val x2cpg         = project.in(frontendsRoot / "x2cpg")
  lazy val pysrc2cpg     = project.in(frontendsRoot / "pysrc2cpg")
  lazy val php2cpg       = project.in(frontendsRoot / "php2cpg")
  lazy val jssrc2cpg     = project.in(frontendsRoot / "jssrc2cpg")
  lazy val swiftsrc2cpg  = project.in(frontendsRoot / "swiftsrc2cpg")
  lazy val javasrc2cpg   = project.in(frontendsRoot / "javasrc2cpg")
  lazy val jimple2cpg    = project.in(frontendsRoot / "jimple2cpg")
  lazy val kotlin2cpg    = project.in(frontendsRoot / "kotlin2cpg")
  lazy val rubysrc2cpg   = project.in(frontendsRoot / "rubysrc2cpg")
  lazy val gosrc2cpg     = project.in(frontendsRoot / "gosrc2cpg")
  lazy val csharpsrc2cpg = project.in(frontendsRoot / "csharpsrc2cpg")

}
