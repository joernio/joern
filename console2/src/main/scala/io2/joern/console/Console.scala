package io2.joern.console

import dotty.tools.repl._

object Console:

  def main(args: Array[String]): Unit =
    // pass classpath on into the repl
    val replArgs = args ++ Array("-classpath", System.getProperty("java.class.path"))
    // new ReplDriver(replArgs).tryRunning
    val repl = new ReplDriver(replArgs)
    val state = repl.initialState
    val state2 = repl.run("def foo = 43")(state)
    val state3 = repl.run("import Int.MaxValue")(state2)
    repl.runUntilQuit(state3)

    
