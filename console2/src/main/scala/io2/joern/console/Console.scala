package io2.joern.console

import dotty.tools.repl._

object Console:

  def main(args: Array[String]): Unit =
    // pass classpath on into the repl
    val replArgs = args ++ Array("-classpath", System.getProperty("java.class.path"))
    // new ReplDriver(replArgs).tryRunning
    val repl = new ReplDriver(replArgs)
    val predefCode =
      """
        |def foo = 42
        |import Int.MaxValue
        |""".stripMargin

    val state = repl.run(predefCode)(repl.initialState)
    repl.runUntilQuit(state)

    
