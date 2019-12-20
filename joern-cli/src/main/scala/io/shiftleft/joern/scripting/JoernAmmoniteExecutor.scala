package io.shiftleft.joern.scripting

import io.shiftleft.console.scripting.AmmoniteExecutor

object JoernAmmoniteExecutor extends AmmoniteExecutor {
  override lazy val predef: String =
    """
      |import gremlin.scala.{`package` => _, _}
      |import io.shiftleft.console.{`package` => _, _}
      |import io.shiftleft.joern.console._
      |import io.shiftleft.joern.console.Console.{cpg => _, _}
      |import io.shiftleft.codepropertygraph.Cpg
      |import io.shiftleft.codepropertygraph.cpgloading._
      |import io.shiftleft.codepropertygraph.generated._
      |import io.shiftleft.codepropertygraph.generated.nodes._
      |import io.shiftleft.codepropertygraph.generated.edges._
      |import io.shiftleft.dataflowengine.language.{`package` => _, _}
      |import io.shiftleft.semanticcpg.language.{`package` => _, _}
      |import scala.jdk.CollectionConverters._
      |implicit val resolver: ICallResolver = NoResolve
      |""".stripMargin
}
