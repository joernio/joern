package io.shiftleft.joern.console

import io.shiftleft.console.{BridgeBase, JoernProduct}

object AmmoniteBridge extends App with BridgeBase {

  runAmmonite(parseConfig(args), JoernProduct)

  /**
    * Code that is executed when starting the shell
    * */
  override def predefPlus(lines: List[String]): String = {
    val default =
      """
        |import gremlin.scala._
        |import io.shiftleft.console._
        |import io.shiftleft.joern.console._
        |import io.shiftleft.joern.console.Console._
        |import io.shiftleft.codepropertygraph.Cpg
        |import io.shiftleft.codepropertygraph.cpgloading._
        |import io.shiftleft.codepropertygraph.generated._
        |import io.shiftleft.codepropertygraph.generated.nodes._
        |import io.shiftleft.codepropertygraph.generated.edges._
        |import io.shiftleft.dataflowengine.language._
        |import io.shiftleft.semanticcpg.language._
        |import scala.jdk.CollectionConverters._
        |implicit val resolver: ICallResolver = NoResolve
        |
      """.stripMargin
    lines.foldLeft(default) { case (res, line) => res + s"\n$line" }
  }

  override def promptStr(): String = "joern> "

  override def shutdownHooks: List[String] = List()

}
