package io.shiftleft.joern.console

import io.shiftleft.console.{BridgeBase, JoernProduct}

object AmmoniteBridge extends App with BridgeBase {

  runAmmonite(parseConfig(args), JoernProduct)

  /**
    * Code that is executed when starting the shell
    * */
  override def predefPlus(lines: List[String]): String = {
    lines.foldLeft(Predefined.forInteractiveShell) { case (res, line) => res + s"\n$line" }
  }

  override def promptStr(): String = "joern> "

  override def shutdownHooks: List[String] = List()

}
