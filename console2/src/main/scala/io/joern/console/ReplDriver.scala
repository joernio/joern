package io.joern.console

import dotty.tools.dotc.core.Contexts.Context
import org.jline.reader._
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

class ReplDriver(args: Array[String]) extends dotty.tools.repl.ReplDriver(args) {

  import dotty.tools.repl._

  /** Run REPL with `state` until `:quit` command found

    * We'd rather override `dotty.ReplDriver.runUntilQuit`, but that's `final`...
    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  def runUntilQuit0(using initialState: State = initialState)(): State = {
    val terminal = new JLineTerminal

    // println(
    //   s"""Welcome to Scala $simpleVersionString ($javaVersion, Java $javaVmName).
    //      |Type in expressions for evaluation. Or try :help.""".stripMargin)

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
        case _: EndOfFileException |
            _: UserInterruptException => // Ctrl+D or Ctrl+C
          Quit
      }
    }

    @tailrec def loop(using state: State)(): State = {
      val res = readLine()
      if (res == Quit) state
      else loop(using interpret(res))()
    }

    // try runBody { loop() }
    // finally terminal.close()
    ???
  }

  /** carbon copy from dotty.ReplDriver, only because it's private... */
  private def interpret(res: ParseResult)(using state: State): State = {
    res match {
      case parsed: Parsed if parsed.trees.nonEmpty =>
        compile(parsed, state)

      // case SyntaxErrors(_, errs, _) =>
      //   displayErrors(errs)
      //   state

      // case cmd: Command =>
      //   interpretCommand(cmd)

      // case SigKill => // TODO
      //   state

      case _ => // new line, empty tree
        state
    }
  }



}
