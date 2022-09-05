package io.joern.console

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.repl.{Newline, ParseResult, State, Quit}
import java.io.PrintStream
import org.jline.reader._
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 onExit: Option[PrintStream => Unit] = None,
                 greeting: String,
                 prompt: String) extends dotty.tools.repl.ReplDriver(args, out) {

  /** Run REPL with `state` until `:quit` command found

    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  override def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal(prompt)

    out.println(greeting)

    /** Blockingly read a line, getting back a parse result */
    def readLine(state: State): ParseResult = {
      val completer: Completer = { (_, line, candidates) =>
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.asJava)
      }
      given Context = state.context
      try {
        val line = terminal.readLine(completer)
        ParseResult(line)(state)
      } catch {
        case _: EndOfFileException => // Ctrl+D
          onExit.foreach(_.apply(out))
          Quit
        case _: UserInterruptException => // Ctrl+C
          Newline
      }
    }

    @tailrec def loop(state: State): State = {
      val res = readLine(state)
      if (res == Quit) state
      else loop(interpret(res)(state))
    }

    try runBody { loop(initialState) }
    finally terminal.close()
  }


}
