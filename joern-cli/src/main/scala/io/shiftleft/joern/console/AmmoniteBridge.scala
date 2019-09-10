package io.shiftleft.joern.console

import io.shiftleft.console.BridgeBase

object AmmoniteBridge extends App with BridgeBase {

  runAmmonite(parseConfig(args))

  /**
    * Code that is executed when starting the shell
    * */
  override def predefPlus(lines: List[String]) : String = {
    val default =
      """
        |import io.shiftleft.joern.console._
        |import io.shiftleft.joern.console.Console._
        |
      """.stripMargin
    lines.foldLeft(default) { case (res, line) => res + s"\n$line" }
  }

  override def promptStr() : String = "joern> "

  override def shutdownHooks : List[String] = List()

}
