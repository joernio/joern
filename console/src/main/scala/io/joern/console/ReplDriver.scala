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



  // // TODO remove debug code
  // import dotty.tools.dotc.ast.Trees._
  // import dotty.tools.dotc.ast.{tpd, untpd}
  // import dotty.tools.dotc.config.CommandLineParser.tokenize
  // import dotty.tools.dotc.config.Properties.{javaVersion, javaVmName, simpleVersionString}
  // import dotty.tools.dotc.core.Contexts._
  // import dotty.tools.dotc.core.Decorators._
  // import dotty.tools.dotc.core.Phases.{unfusedPhases, typerPhase}
  // import dotty.tools.dotc.core.Denotations.Denotation
  // import dotty.tools.dotc.core.Flags._
  // import dotty.tools.dotc.core.Mode
  // import dotty.tools.dotc.core.NameKinds.SimpleNameKind
  // import dotty.tools.dotc.core.NameKinds.DefaultGetterName
  // import dotty.tools.dotc.core.NameOps._
  // import dotty.tools.dotc.core.Names.Name
  // import dotty.tools.dotc.core.StdNames._
  // import dotty.tools.dotc.core.Symbols.{Symbol, defn}
  // import dotty.tools.dotc.interfaces
  // import dotty.tools.dotc.interactive.Completion
  // import dotty.tools.dotc.printing.SyntaxHighlighting
  // import dotty.tools.dotc.reporting.{ConsoleReporter, StoreReporter}
  // import dotty.tools.dotc.reporting.Diagnostic
  // import dotty.tools.dotc.util.Spans.Span
  // import dotty.tools.dotc.util.{SourceFile, SourcePosition}
  // import dotty.tools.dotc.{CompilationUnit, Driver}
  // import dotty.tools.dotc.config.CompilerCommand
  // import dotty.tools.dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}
  // import dotty.tools.repl.{ReplCompiler}

  // def runQuietly(input: String)(implicit state: State): State =
  //   runBody {
  //     val parsed = ParseResult(input)(state)
  //     val res = interpret(parsed)
  //     res
  //   }

  // override protected def interpret(res: ParseResult)(implicit state: State): State = {
  //   import dotty.tools.repl._
  //   res match {
  //     case parsed: Parsed if parsed.trees.nonEmpty =>
  //       compile(parsed, state)

  //     case SyntaxErrors(_, errs, _) =>
  //       // displayErrors(errs)
  //       // state
  //       ???

  //     case cmd: Command =>
  //       // interpretCommand(cmd)
  //       ???

  //     case SigKill => // TODO
  //       state

  //     case _ => // new line, empty tree
  //       state
  //   }
  // }

  // /** Compile `parsed` trees and evolve `state` in accordance */
  // private def compile(parsed: Parsed, istate: State): State = {
  //   def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
  //     case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
  //     case _ => nme.NO_NAME
  //   }

  //   def extractTopLevelImports(ctx: Context): List[tpd.Import] =
  //     unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

  //   def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
  //     if imports.isEmpty then ctx
  //     else
  //       imports.foldLeft(ctx.fresh.setNewScope)((ctx, imp) =>
  //         ctx.importContext(imp, imp.symbol(using ctx)))

  //   implicit val state = {
  //     val state0 = newRun(istate, parsed.reporter)
  //     state0.copy(context = state0.context.withSource(parsed.source))
  //   }
  //   compiler
  //     .compile(parsed)
  //     .fold(
  //       displayErrors,
  //       {
  //         case (unit: CompilationUnit, newState: State) =>
  //           val newestWrapper = extractNewestWrapper(unit.untpdTree)
  //           val newImports = extractTopLevelImports(newState.context)
  //           var allImports = newState.imports
  //           if (newImports.nonEmpty)
  //             allImports += (newState.objectIndex -> newImports)
  //           val newStateWithImports = newState.copy(
  //             imports = allImports,
  //             context = contextWithNewImports(newState.context, newImports)
  //           )

  //           val warnings = newState.context.reporter
  //             .removeBufferedMessages(using newState.context)


  //           inContext(newState.context) {
  //             val (updatedState, definitions) =
  //               if (!ctx.settings.XreplDisableDisplay.value)
  //                 renderDefinitions(unit.tpdTree, newestWrapper)(newStateWithImports)
  //               else
  //                 (newStateWithImports, Seq.empty)

  //             // output is printed in the order it was put in. warnings should be
  //             // shown before infos (eg. typedefs) for the same line. column
  //             // ordering is mostly to make tests deterministic
  //             implicit val diagnosticOrdering: Ordering[Diagnostic] =
  //               Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

  //             (definitions ++ warnings)
  //               .sorted
  //               .foreach(printDiagnostic)

  //             updatedState
  //           }
  //       }
  //     )
  // }

  // private def newRun(state: State, reporter: StoreReporter = newStoreReporter) = {
  //   val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
  //   state.copy(context = run.runContext)
  // }

  // private def newStoreReporter: StoreReporter =
  //   new StoreReporter(null)
  //   with UniqueMessagePositions with HideNonSensicalMessages

  // private def displayErrors(errs: Seq[Diagnostic])(implicit state: State): State = {
  //   errs.foreach(printDiagnostic)
  //   state
  // }

  // /** Like ConsoleReporter, but without file paths, -Xprompt displaying,
  //  *  and using a PrintStream rather than a PrintWriter so messages aren't re-encoded. */
  // private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter {
  //   override def posFileStr(pos: SourcePosition) = "" // omit file paths
  //   override def printMessage(msg: String): Unit = out.println(msg)
  //   override def flush()(using Context): Unit    = out.flush()
  // }

}
