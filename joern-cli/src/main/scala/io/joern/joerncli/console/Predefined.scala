package io.joern.joerncli.console

import io.joern.console.{Help, Run}

object Predefined {

  /**
    * TODO bring back this one - removed temporarily because the `overflowdb.traversal.toXyzViaAdditionalImplicit` cause autocompletion to crash the entire repl...
      |import overflowdb.traversal._
    */
  val shared: String =
    """
      |import _root_.io.joern.console._
      |import _root_.io.joern.joerncli.console.JoernConsole._
      |import _root_.io.shiftleft.codepropertygraph.Cpg
      |import _root_.io.shiftleft.codepropertygraph.Cpg.docSearchPackages
      |import _root_.io.shiftleft.codepropertygraph.cpgloading._
      |import _root_.io.shiftleft.codepropertygraph.generated._
      |import _root_.io.shiftleft.codepropertygraph.generated.nodes._
      |import _root_.io.shiftleft.codepropertygraph.generated.edges._
      |import _root_.io.joern.dataflowengineoss.language._
      |import _root_.io.shiftleft.semanticcpg.language._
      |import overflowdb._
      |import scala.jdk.CollectionConverters._
      |implicit val resolver: ICallResolver = NoResolve
      |implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
      """.stripMargin

  val forInteractiveShell: String =
    shared +
      """
        |import _root_.io.joern.joerncli.console.Joern._
        |def script(x: String) : Any = console.runScript(x, Map(), cpg)
      """.stripMargin +
      dynamicPredef()

  val forScripts: String =
    shared +
      """
        |import _root_.io.joern.joerncli.console.Joern.{cpg =>_, _}
      """.stripMargin +
      dynamicPredef()

  def dynamicPredef(): String = {
    Run.codeForRunCommand() +
      Help.codeForHelpCommand(classOf[io.joern.joerncli.console.JoernConsole])
  }

}
