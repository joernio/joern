package io.joern.console

import flatgraph.help.DocFinder.*
import flatgraph.help.Table.AvailableWidthProvider
import flatgraph.help.{DocFinder, Table}

object Help {

  def overview(clazz: Class[?])(using AvailableWidthProvider): String = {
    val columnNames = List("command", "description", "example")
    val rows = DocFinder
      .findDocumentedMethodsOf(clazz)
      .map { case StepDoc(_, funcName, doc) =>
        List(funcName, doc.info, doc.example)
      }
      .toList ++ List(runRow)

    val header = formatNoQuotes("""
      |
      |Welcome to the interactive help system. Below you find
      |a table of all available top-level commands. To get
      |more detailed help on a specific command, just type
      |
      |`help.<command>`.
      |
      |Try `help.importCode` to begin with.
      |
      |
      |""".stripMargin)
    header + "\n" + Table(columnNames, rows.sortBy(_.head)).render
  }

  def format(text: String): String = {
    "\"\"\"" + "\n" + formatNoQuotes(text) + "\"\"\""
  }

  def formatNoQuotes(text: String): String = {
    text.stripMargin
      .split("\n\n")
      .mkString("\n\n")
      .trim
  }

  private def runRow: List[String] =
    List("run", "Run analyzer on active CPG", "run.securityprofile")

  // Since `run` is generated dynamically, it's not picked up when looking
  // through methods via reflection, and therefore, we are adding
  // it manually.
  def runLongHelp: String =
    Help.format("""
        |
        |""".stripMargin)

  def codeForHelpCommand(clazz: Class[?]): String = {
    val membersCode = DocFinder
      .findDocumentedMethodsOf(clazz)
      .map { case StepDoc(_, funcName, doc) =>
        s"    val $funcName: String = ${Help.format(doc.longInfo)}"
      }
      .mkString("\n")

    val overview = Help.overview(clazz)
    s"""
       | class Helper() {
       |   def run: String = Help.runLongHelp
       |   override def toString: String = \"\"\"$overview\"\"\"
       |
       |  $membersCode
       | }
       |
       | val help = new Helper
       |""".stripMargin
  }

}
