import sbt._

object Projects {
  lazy val joerncli = project.in(file("joern-cli"))
  lazy val querydb = project.in(file("querydb"))
  lazy val console = project.in(file("console"))
}
