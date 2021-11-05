package io.joern.console

import io.joern.console.{Help, Run}

object Predefined {

  /* ammonite tab completion is partly broken for scala > 2.12.8
   * applying workaround for package wildcard imports from https://github.com/lihaoyi/Ammonite/issues/1009 */
  val shared: String = """
        |import io.joern.console.{`package` => _, _}
        |import io.joern.console.JoernConsole._
        |import io.shiftleft.codepropertygraph.Cpg
        |import io.shiftleft.codepropertygraph.cpgloading._
        |import io.shiftleft.codepropertygraph.generated._
        |import io.shiftleft.codepropertygraph.generated.nodes._
        |import io.shiftleft.codepropertygraph.generated.edges._
        |import io.shiftleft.dataflowengineoss.language.{`package` => _, _}
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
        |import io.joern.console.Joern._
      """.stripMargin +
      dynamicPredef()

  val forScripts: String =
    shared +
      """
        |import io.joern.console.Joern.{cpg =>_, _}
      """.stripMargin +
      dynamicPredef()

  def dynamicPredef(): String = {
    Run.codeForRunCommand() +
      Help.codeForHelpCommand[io.joern.console.JoernConsole]
  }

}
