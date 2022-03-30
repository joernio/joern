import sbt._

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  lazy val joerncli          = project.in(file("joern-cli"))
  lazy val querydb           = project.in(file("querydb"))
  lazy val console           = project.in(file("console"))
  lazy val dataflowengineoss = project.in(file("dataflowengineoss"))
  lazy val macros            = project.in(file("macros"))
  lazy val semanticcpg       = project.in(file("semanticcpg"))

  lazy val c2cpg      = project.in(frontendsRoot / "c2cpg")
  lazy val ghidra2cpg = project.in(frontendsRoot / "ghidra2cpg")
  lazy val fuzzyc2cpg = project.in(frontendsRoot / "fuzzyc2cpg")
  lazy val x2cpg      = project.in(frontendsRoot / "x2cpg")
  lazy val pysrc2cpg  = project.in(frontendsRoot / "pysrc2cpg")
  lazy val php2cpg    = project.in(frontendsRoot / "php2cpg")
}
