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
                 prompt: String) extends dotty.tools.repl.ReplDriver(args, out) {

  override def initCtx: Context = {
    val x = super.initCtx
    val maxPrintElementsSetting: dotty.tools.dotc.config.Settings.Setting[Int] = x.settings.XreplMaxPrintElements
    val newContext = x.fresh.setSetting(maxPrintElementsSetting, 2000)
    println("XX2:" + maxPrintElementsSetting.valueIn(x.settingsState))
    newContext
  }

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


  // TODO drop debug stuff
  import dotty.tools.dotc.reporting.{ConsoleReporter, Diagnostic}
  import dotty.tools.dotc.interfaces
  import dotty.tools.dotc.util.SourcePosition

  /** Like ConsoleReporter, but without file paths, -Xprompt displaying,
   *  and using a PrintStream rather than a PrintWriter so messages aren't re-encoded. */
  private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter {
    override def posFileStr(pos: SourcePosition) = "" // omit file paths
    override def printMessage(msg: String): Unit = out.println(msg)
    override def flush()(using Context): Unit    = out.flush()
  }

  /** Print warnings & errors using ReplConsoleReporter, and info straight to out */
  override protected def printDiagnostic(dia: Diagnostic)(implicit state: State) = dia.level match
    case interfaces.Diagnostic.INFO => out.println(dia.msg) // print REPL's special info diagnostics directly to out
    case _                          => ReplConsoleReporter.doReport(dia)(using state.context)




}
