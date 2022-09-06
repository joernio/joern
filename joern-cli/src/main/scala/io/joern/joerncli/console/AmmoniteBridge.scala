package io.joern.joerncli.console

import io.joern.console.{BridgeBase, JoernProduct}

import java.io.PrintStream

object AmmoniteBridge extends BridgeBase {

  override val slProduct = JoernProduct

  def main(args: Array[String]): Unit = {
    runAmmonite(parseConfig(args))
  }

  /** Code that is executed when starting the shell
    */
  override def predefPlus(lines: List[String]): String = {
    lines.foldLeft(Predefined.forInteractiveShell) { case (res, line) => res + s"\n$line" }
  }

  override def greeting = JoernConsole.banner()

  override def promptStr: String = "joern> "

  override def onExitCode: String = "workspace.projects.foreach(_.close)"

}
