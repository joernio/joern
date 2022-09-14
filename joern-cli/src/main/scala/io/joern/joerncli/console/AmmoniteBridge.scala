package io.joern.joerncli.console

import io.joern.console.{BridgeBase, JoernProduct}

object AmmoniteBridge extends App with BridgeBase {

  runAmmonite(parseConfig(args), JoernProduct)


  /** Code that is executed when starting the shell
    */
  override def predefPlus(lines: List[String]): String = {
    // TODO cleanup, find simple solution, e.g. `def isServer` or `def reportOutStream` in BridgeBase
    println("XXXXXXXX1")
    // lines.foldLeft(Predefined.forInteractiveShell) { case (res, line) => res + s"\n$line" }
    Predefined.forInteractiveShell) { case (res, line) => res + s"\n$line" }
    s"""
      |import io.joern.console.{`package` => _, _}
      |import io.joern.joerncli.console.JoernConsole._
      |import io.shiftleft.codepropertygraph.Cpg
      |import io.shiftleft.codepropertygraph.Cpg.docSearchPackages
      |import io.shiftleft.codepropertygraph.cpgloading._
      |import io.shiftleft.codepropertygraph.generated._
      |import io.shiftleft.codepropertygraph.generated.nodes._
      |import io.shiftleft.codepropertygraph.generated.edges._
      |import io.joern.dataflowengineoss.language.{`package` => _, _}
      |import io.shiftleft.semanticcpg.language.{`package` => _, _}
      |import overflowdb.{`package` => _, _}
      |import overflowdb.traversal.{`package` => _, help => _, _}
      |import scala.jdk.CollectionConverters._
      |implicit val resolver: ICallResolver = NoResolve
      |implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
      |import io.joern.joerncli.console.Joern._
      |def script(x: String) : Any = console.runScript(x, Map(), cpg)
      |${lines.mkString("\n")}
    """.stripMargin
  }

  override def promptStr(): String = "joern> "

  override def shutdownHooks: List[String] = List("""interp.beforeExitHooks.append{_ =>
      |println("Would you like to save changes? (y/N)")
      |val answer = scala.Console.in.read.toChar
      |if (answer == 'Y' || answer == 'y') {
      |  System.err.println("saving.")
      |  workspace.projects.foreach { p =>
      |        p.close
      |  }
      | }
      |}
      |""".stripMargin)

}
