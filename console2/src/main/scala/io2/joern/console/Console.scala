package io2.joern.console

import dotty.tools.repl._

object Console:

  def main(args: Array[String]): Unit =
    // pass classpath on into the repl
    val replArgs = args ++ Array("-classpath", System.getProperty("java.class.path"))
    // new ReplDriver(replArgs).tryRunning
    val repl = new ReplDriver(replArgs)
    val state = repl.initialState
    println("imports: " + state.imports)
    println(state.valIndex)
    println(state.objectIndex)
    val state2 = repl.run("def foo = 42")(state)
    println(state2.valIndex)
    println(state2.objectIndex)
    repl.runUntilQuit(state2)

    
