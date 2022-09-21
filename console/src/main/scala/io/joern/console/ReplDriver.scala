package io.joern.console

import dotty.tools.MainGenericCompiler.classpathSeparator
import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, ContextState, FreshContext, ctx}
import dotty.tools.dotc.ast.{Positioned, tpd, untpd}
import dotty.tools.dotc.classpath.{AggregateClassPath, ClassPathFactory}
import dotty.tools.dotc.config.{Feature, JavaPlatform, Platform}
import dotty.tools.dotc.core.{Contexts, MacroClassLoader, Mode, TyperState}
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation}
import dotty.tools.repl.{CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}

import java.io.PrintStream
import org.jline.reader.*

import java.net.URL
import javax.naming.InitialContext
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object HackyGlobalState {
  val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
  var calledUsing = false
}

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 onExitCode: Option[String] = None,
                 greeting: String,
                 prompt: String,
                 maxPrintElements: Int,
                 classLoader: Option[ClassLoader] = None) extends dotty.tools.repl.ReplDriver(args, out, classLoader) {
  import HackyGlobalState.versionSortJar

  override def initCtx: Context = {
    val ctx = super.initCtx

    val base: ContextBase = new ContextBase {
      override def newPlatform(using Context): Platform = {
        println(s"initCtx: creating new platform; calledUsing=${HackyGlobalState.calledUsing}")
        new JavaPlatform {
          override def classPath(using Context): ClassPath = {
            println(s"initial javaplatform -> classpath; calledUsing=${HackyGlobalState.calledUsing}")
//            val oldScope = ctx.scope // always empty
            val original = super.classPath
            val versionSortClassPath = ClassPathFactory.newClassPath(AbstractFile.getFile(versionSortJar))
//            val extJarsDir = "/home/mp/Projects/shiftleft/joern/extjars"
//            val extClassesDir = "/home/mp/Projects/shiftleft/joern/extclasses"
//            val directoryClassPath = ClassPathFactory.newClassPath(AbstractFile.getDirectory(extClassesDir))
            val cpResult = if (HackyGlobalState.calledUsing) Seq(original, versionSortClassPath) else Seq(original)
            new AggregateClassPath(cpResult) {
//              override def list(inPackage: String) = {
//                println("AggregateClassPath.list")
//                if (HackyGlobalState.calledUsing) throw new AssertionError("boom") //who's calling us?
//                else super.list(inPackage)
//              }
            }
          }
        }
      }
    }

    println(s"ReplDriver.initCtx. calledUsing=${HackyGlobalState.calledUsing}")
    new Contexts.InitialContext(base, ctx.settings)
  }

  /** Run REPL with `state` until `:quit` command found
    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  override def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal(prompt)
    initializeRenderer()

    out.println(greeting)

    /** Blockingly read a line, getting back a parse result */
    def readLine(state0: State): ParseResult = {
      val state =
        if (HackyGlobalState.calledUsing) {
          println("called `using` before - fiddling with state")
          val oldCtx = state0.context
          val newCp = s"${oldCtx.settings.classpath.value(using oldCtx)}$versionSortJar$classpathSeparator"
          val ctx1 = oldCtx.fresh.setSetting(oldCtx.settings.classpath, newCp)
          val ctx2 = ctx1.setNewScope.setNewTyperState()
//          ctx2.typerState.fresh()
//          ctx1.initialize()(using ctx1)
          state0.copy(context = ctx2)
        } else {
          state0
        }
      val completer: Completer = { (_, line, candidates) =>
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.asJava)
      }
      given Context = state.context
      try {
        val line = terminal.readLine(completer)
        if (line.startsWith("//> using")) {
          HackyGlobalState.calledUsing = true

          rootCtx = {
            val oldCtx: FreshContext = rootCtx.asInstanceOf[FreshContext]
            val baseCtx: ContextBase = new ContextBase {
              override def newPlatform(using Context): Platform = {
                new JavaPlatform {
                  override def classPath(using Context): ClassPath = {
                    val original = super.classPath
                    val versionSortClassPath = ClassPathFactory.newClassPath(AbstractFile.getFile(versionSortJar))
                    val cpResult = if (HackyGlobalState.calledUsing) Seq(original, versionSortClassPath) else Seq(original)
                    new AggregateClassPath(cpResult)
                  }
                }
              }
            }
//
//            // TODO how can i connect the two? idea: create a new FreshContext, copy (almost) everything over
//            oldCtx.withTyperState(TyperState.initialState())
//            // rootCtx.fresh // maintains history, but doesn't use new platform
            val newRootCtx = new Contexts.InitialContext(baseCtx, rootCtx.settings) // works, but loses history
            command.distill(args, newRootCtx.settings)(newRootCtx.settingsState)(using newRootCtx)
            /* must call ^ - otherwise:
            Exception in thread "main" dotty.tools.dotc.MissingCoreLibraryException: Could not find package scala from compiler core libraries.
            Make sure the compiler core libraries are on the classpath. */

//            oldCtx.base.reset()
//            oldCtx
//              println(s"XXX5 cp=${oldCtx.settings.classpath.value}")
//              val newCtx = oldCtx.setSetting(oldCtx.settings.classpath, s"${oldCtx.settings.classpath.value}$classpathSeparator$versionSortJar")
//oldCtx.settings.classpath.updateIn()
            // TODO call `fromTastySetup(additionalFiles)
            val ctx1 = oldCtx.fresh
            val newCp = s"${oldCtx.settings.classpath.value}$versionSortJar$classpathSeparator"
//            ctx1.settings.classpath.updateIn(ctx1.settings, versionSortJar)
            val ctx2 = ctx1.setSetting(ctx1.settings.classpath, newCp)
            //            val newCtx = oldCtx.fresh.setSetting(oldCtx.settings.classpath, s"${oldCtx.settings.classpath.value}$classpathSeparator$versionSortJar")
            println(s"XXX6 new cp=${ctx2.settings.classpath.value}")
            println(s"XXX7 settingsState: ${ctx2.settingsState}")
//            println(s"oldRootCtx class=${rootCtx.getClass}") // FreshContext
//            println(s"newRootCtx class=${newRootCtx.getClass}") //InitialContext
//            newRootCtx.freshOver(rootCtx) // no workie
//            rootCtx.freshOver(newRootCtx) // doesn't work either...
            newRootCtx
          }

          rendering.myClassLoader = null

          ParseResult(line)(state)
          //          ParseResult(line)(initialState)

//          this.compiler.reset() // that alone doesn't work...
//          compiler = new dotty.tools.repl.ReplCompiler // this trips up everything...
        }
        else {
          ParseResult(line)(state)
        }
      } catch {
        case _: EndOfFileException => // Ctrl+D
//          onExitCode.foreach(code => run(code)(state))
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
