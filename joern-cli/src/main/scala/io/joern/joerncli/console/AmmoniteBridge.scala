package io.joern.joerncli.console

import io.joern.console.{BridgeBase, JoernProduct}

object AmmoniteBridge extends BridgeBase {

  def main(args: Array[String]): Unit = {
    runAmmonite(parseConfig(args), JoernProduct)
  }


  /** Code that is executed when starting the shell
    */
  override def predefPlus(lines: List[String]): String = {
    s"""${Predefined.forInteractiveShell}
       |${lines.mkString("\n")}
       |""".stripMargin
  }

  override def promptStr(): String = "joern> "

  override def shutdownHooks: List[String] = List("""interp.beforeExitHooks.append{_ =>
      |println("Would you like to save changes? (y/N)")
      |val answer = scala.Console.in.read.toChar
      |if (answer == 'Y' || answer == 'y') {
      |  System.err.println("saving.")
      |  workspace.projects.foreach { p =>
      |        p.close
      |  }
      | }
      |}
      |""".stripMargin)

}
