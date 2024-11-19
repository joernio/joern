package io.joern.console

import flatgraph.help.DocFinder.*
import flatgraph.help.Table.{AvailableWidthProvider, Row}
import flatgraph.help.{DocFinder, Table}

object Help {

  /** allows users to extend the help table with additional entries */
  val additionalHelpEntries = Seq.newBuilder[Tuple3[String, String, String]]

  def overview(clazz: Class[?])(using AvailableWidthProvider): String = {
    val columnNames = List("command", "description", "example")

    val rows = Seq.newBuilder[Row]
    rows += runRow
    DocFinder.findDocumentedMethodsOf(clazz).foreach { case StepDoc(_, funcName, doc) =>
      rows += List(funcName, doc.info, doc.example)
    }
    additionalHelpEntries.result().foreach { case (a, b, c) =>
      rows += List(a, b, c)
    }

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
    header + "\n" + Table(columnNames, rows.result().sortBy(_.head)).render
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
  // through methods via reflection, and therefore, we are adding it manually.
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

    s"""
       | class Helper() {
       |   def run: String = Help.runLongHelp
       |   override def toString: String = Help.overview(classOf[${clazz.getName}])
       |
       |  $membersCode
       | }
       |
       | val help = new Helper
       |""".stripMargin
  }

}
