package io.joern.joerncli.console

import io.joern.console.{Help, Run}

object Predefined {

  /* ammonite tab completion is partly broken for scala > 2.12.8
   * applying workaround for package wildcard imports from https://github.com/lihaoyi/Ammonite/issues/1009 */
  val shared: String =
    """
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
      """.stripMargin

  val forInteractiveShell: String =
    shared +
      """
        |import io.joern.joerncli.console.Joern._
        |def script(x: String) : Any = console.runScript(x, Map(), cpg)
      """.stripMargin +
      dynamicPredef()

  val forScripts: String =
    shared +
      """
        |import io.joern.joerncli.console.Joern.{cpg =>_, _}
      """.stripMargin +
      dynamicPredef()

  def dynamicPredef(): String = {
    Run.codeForRunCommand() +
      Help.codeForHelpCommand(classOf[io.joern.joerncli.console.JoernConsole])
  }

}
