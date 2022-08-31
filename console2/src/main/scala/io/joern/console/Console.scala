package io.joern.console

import dotty.tools.repl.State

object Console {

  def bar = "barz"

  val predefCode =
    """
      |def foo = 42
      |import Int.MaxValue
      |import _root_.io.joern.console.Console.bar
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val additionalArgs = Array(
      "-classpath", // pass classpath on into the repl
      System.getProperty("java.class.path"),
      "-explain", // verbose scalac error messages
    )
    val greeting = "hey there!"
    val repl = new ReplDriver(
      args ++ additionalArgs,
      scala.Console.out,
      greeting)

    val stateAfterPredef = repl.run(predefCode)(using repl.initialState)
    repl.runUntilQuit(using stateAfterPredef)()
  }

}
