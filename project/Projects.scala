import sbt._

object Projects {
  lazy val joerncli = project.in(file("joern-cli"))
  lazy val ghidra2cpg = project.in(file("ghidra2cpg-build"))
}
