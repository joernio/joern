package io2.joern.console

import dotty.tools.repl._

object Console {

  val predefCode =
    """
      |def foo = 42
      |import Int.MaxValue
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    // pass classpath on into the repl
    val replArgs = args ++ Array("-classpath", System.getProperty("java.class.path"))
    val repl = new ReplDriver(replArgs)

    val stateAfterPredef = repl.run(predefCode)(repl.initialState)
    repl.runUntilQuit(stateAfterPredef)
  }

}
