package io.joern.console

import dotty.tools.repl._

object Console {

  def bar = "barz"

  val predefCode =
    """
      |def foo = 42
      |import Int.MaxValue
      |import _root_.io.joern.console.Console.bar
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    // pass classpath on into the repl
    val replArgs = args ++ Array("-classpath", System.getProperty("java.class.path"))
    val repl = new ReplDriver(replArgs)

    val stateAfterPredef = repl.run(predefCode)(repl.initialState)
    repl.runUntilQuit(stateAfterPredef)
  }

}
