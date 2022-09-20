package io.joern.console

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.ast.{Positioned, tpd, untpd}
import dotty.tools.dotc.classpath.{AggregateClassPath, ClassPathFactory}
import dotty.tools.dotc.config.{Feature, JavaPlatform, Platform}
import dotty.tools.dotc.core.{Contexts, MacroClassLoader, Mode}
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation}
import dotty.tools.repl.{CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}

import java.io.PrintStream
import org.jline.reader.*

import java.net.URL
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 onExitCode: Option[String] = None,
                 greeting: String,
                 prompt: String,
                 maxPrintElements: Int,
                 classLoader: Option[ClassLoader] = None) extends dotty.tools.repl.ReplDriver(args, out, classLoader) {

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
        // TODO extract, handle elsewhere
        if (line.startsWith("//> using")) {
          val settings = args
          import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
          import Contexts.ctx
          def setup(args: Array[String], rootCtx: Context): Context = {
            val oldScope = rootCtx.scope

            val ictx = rootCtx
            val cmdDistill = command.distill(args, ictx.settings)(ictx.settingsState)(using ictx)
            ictx
          }
//          val newRootCtx = this.initCtx
          val newRootCtx = {
            val ctx = super.initCtx
            val base: ContextBase = new ContextBase {
              override def newPlatform(using Context): Platform = {
                new JavaPlatform {
                  override def classPath(using Context): ClassPath = {
                    val original = super.classPath
                    val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
                    val versionSortClassPath = ClassPathFactory.newClassPath(AbstractFile.getFile(versionSortJar))
                    val cpResult = Seq(original, versionSortClassPath)
                    new AggregateClassPath(cpResult)
                  }
                }
              }
            }
            new Contexts.InitialContext(base, ctx.settings)
          }
          rootCtx = setup(settings, newRootCtx)
          rendering.myClassLoader = null

          ParseResult(line)(state)

//          this.compiler.reset() // that alone doesn't work...
//          compiler = new dotty.tools.repl.ReplCompiler // this trips up everything...
        }
        else {
          ParseResult(line)(state)
        }
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
    // TODO reactivate...
    // rendering.myReplStringOf = {
    //   // We need to use the PPrinter class from the on the user classpath, and not the one available in the current
    //   // classloader, so we use reflection instead of simply calling `io.joern.console.PPrinter:apply`.
    //   // This is analogous to what happens in dotty.tools.repl.Rendering.
    //   val pprinter = Class.forName("io.joern.console.PPrinter", true, rendering.myClassLoader)
    //   val renderer = pprinter.getMethod("apply", classOf[Object])
    //   (value: Object) => renderer.invoke(null, value).asInstanceOf[String]
    // }
  }

}
