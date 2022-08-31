package io.joern.console

import dotty.tools.dotc.core.Contexts.Context
import java.io.PrintStream
import org.jline.reader._
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 greeting: String) extends dotty.tools.repl.ReplDriver(args, out) {
  import dotty.tools.repl._

  /** Run REPL with `state` until `:quit` command found

    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  override def runUntilQuit(using initialState: State = initialState)(): State = {
    val terminal = new JLineTerminal

    out.println(greeting)

    /** Blockingly read a line, getting back a parse result */
    def readLine()(using state: State): ParseResult = {
      val completer: Completer = { (_, line, candidates) =>
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.asJava)
      }
      given Context = state.context
      try {
        val line = terminal.readLine(completer)
        ParseResult(line)
      } catch {
        case _: EndOfFileException => // Ctrl+D
          out.println("bye!")
          Quit
        case _: UserInterruptException => // Ctrl+C
          Newline
      }
    }

    @tailrec def loop(using state: State)(): State = {
      val res = readLine()
      if (res == Quit) state
      else loop(using interpret(res))()
    }

    try runBody { loop() }
    finally terminal.close()
  }


}
