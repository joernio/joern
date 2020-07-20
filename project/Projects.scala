import sbt._

object Projects {
  lazy val joerncli = project.in(file("joern-cli"))
  lazy val schemaExtender = project.in(file("schema-extender"))
}
