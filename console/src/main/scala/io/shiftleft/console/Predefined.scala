package io.shiftleft.console

object Predefined {

  /* ammonite tab completion is partly broken for scala > 2.12.8
   * applying workaround for package wildcard imports from https://github.com/lihaoyi/Ammonite/issues/1009 */
  val shared: String = """
        |import io.shiftleft.console.{`package` => _, _}
        |import io.shiftleft.codepropertygraph.Cpg
        |import io.shiftleft.codepropertygraph.cpgloading._
        |import io.shiftleft.codepropertygraph.generated._
        |import io.shiftleft.codepropertygraph.generated.nodes.{`package` => _, _}
        |import io.shiftleft.codepropertygraph.generated.edges._
        |import io.shiftleft.semanticcpg.language.{`package` => _, _}
        |import scala.jdk.CollectionConverters._
        |
      """.stripMargin

  val forScripts: String = shared +
    """
      |import io.shiftleft.joern.console.Joern.{cpg =>_, _}
  """.stripMargin

}
