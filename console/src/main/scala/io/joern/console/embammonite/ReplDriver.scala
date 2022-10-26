package io.joern.console.embammonite

import dotty.tools.dotc.core.Contexts.{Context, ContextBase, ContextState, FreshContext, ctx}
import dotty.tools.repl.{AbstractFileClassLoader, CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}

import java.io.{BufferedReader, InputStream, InputStreamReader, PrintStream}
import scala.annotation.tailrec

class ReplDriver(compilerArgs: Array[String],
                 in: InputStream,
                 out: PrintStream = scala.Console.out,
                 classLoader: Option[ClassLoader] = None) extends dotty.tools.repl.ReplDriver(compilerArgs, out, classLoader) {
  val reader = new BufferedReader(new InputStreamReader(in))

  /** Run REPL with `state` until `:quit` command found
    * Main difference to the 'original': different greeting, trap Ctrl-c
    */
  override def runUntilQuit(initialState: State = initialState): State = {
    /** Blockingly read a line, getting back a parse result */
    def readLine(state: State): ParseResult = {
      given Context = state.context

      try {
        val line = reader.readLine()
        ParseResult(line)(using state)
      } catch {
        case e =>
          e.printStackTrace()
          println(s"caught exception $e with msg=${e.getMessage} ^ - continuing anyway...")
          Newline
      }
    }

    @tailrec def loop(using state: State)(): State = {
      val res = readLine(state)
      if (res == Quit) state
      else loop(using interpret(res))()
    }

    runBody {
      loop(using initialState)()
    }
  }
}
