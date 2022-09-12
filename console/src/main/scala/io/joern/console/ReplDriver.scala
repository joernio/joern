package io.joern.console

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.repl.{CollectTopLevelImports, Newline, Parsed, ParseResult, State, Quit}
import java.io.PrintStream
import org.jline.reader._
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 onExitCode: Option[String] = None,
                 greeting: String,
                 prompt: String,
                 maxPrintElements: Int) extends dotty.tools.repl.ReplDriver(args, out) {

  override def initCtx: Context = {
    val ctx = super.initCtx
    ctx.fresh.setSetting(ctx.settings.XreplMaxPrintElements, maxPrintElements)
  }

  /** Run REPL with `state` until `:quit` command found
    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  override def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal(prompt)
    initializeRenderer()

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
          onExitCode.foreach(code => run(code)(state))
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
  
  /** configure rendering to use our pprinter for displaying results */
  private def initializeRenderer() = {
    rendering.myReplStringOf = {
      // We need to use the PPrinter class from the on the user classpath, and not the one available in the current
      // classloader, so we use reflection instead of simply calling `io.joern.console.PPrinter:apply`.
      // This is analogous to what happens in dotty.tools.repl.Rendering.
      val pprinter = Class.forName("io.joern.console.PPrinter", true, rendering.myClassLoader)
      val renderer = pprinter.getMethod("apply", classOf[Object])
      (value: Object) => renderer.invoke(null, value).asInstanceOf[String]
    }
  }


}
