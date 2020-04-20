package io.shiftleft.joern

object Predefined {

  /* ammonite tab completion is partly broken for scala > 2.12.8
   * applying workaround for package wildcard imports from https://github.com/lihaoyi/Ammonite/issues/1009 */
  val shared: String = """
        |import gremlin.scala.{`package` => _, _}
        |import io.shiftleft.console.{`package` => _, _}
        |import io.shiftleft.joern.console._
        |import io.shiftleft.codepropertygraph.Cpg
        |import io.shiftleft.codepropertygraph.cpgloading._
        |import io.shiftleft.codepropertygraph.generated._
        |import io.shiftleft.codepropertygraph.generated.nodes._
        |import io.shiftleft.codepropertygraph.generated.edges._
        |import io.shiftleft.dataflowengine.language.{`package` => _, _}
        |import io.shiftleft.semanticcpg.language.{`package` => _, _}
        |import scala.jdk.CollectionConverters._
        |implicit val resolver: ICallResolver = NoResolve
        |
      """.stripMargin

  val forInteractiveShell: String = shared +
    """
      | import io.shiftleft.joern.console.JoernConsole._
    """.stripMargin

  val forScripts: String = shared +
    """
    | import io.shiftleft.joern.console.JoernConsole.{cpg => _, _}
  """.stripMargin

}
