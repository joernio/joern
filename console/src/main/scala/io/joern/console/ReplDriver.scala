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
  var jp: JavaPlatform = null
  var initialCp: ClassPath = null
  var calledUsing = false
//  var swap = false
//  var classloader: ClassLoader = null
}

class ReplDriver(args: Array[String],
                 out: PrintStream = scala.Console.out,
                 onExitCode: Option[String] = None,
                 greeting: String,
                 prompt: String,
                 maxPrintElements: Int,
                 classLoader: Option[ClassLoader] = None) extends dotty.tools.repl.ReplDriver(args, out, classLoader) {

//  private val additionalDependencyJars: mutable.Set[String] = mutable.Set.empty

//  def addDependency(jarPath: String): Unit = additionalDependencyJars.add(jarPath)

  override def initCtx: Context = {
    val ctx = super.initCtx
//    ctx.fresh.setSetting(ctx.settings.VreplMaxPrintElements, maxPrintElements)

//    val base: ContextBase = ctx.base
    val base: ContextBase = new ContextBase {
      override def newPlatform(using Context): Platform = {
        println(s"initCtx: creating new platform; calledUsing=${HackyGlobalState.calledUsing}")
        val jp = new JavaPlatform {
          override def classPath(using Context): ClassPath = {
            println(s"initial javaplatform -> classpath; calledUsing=${HackyGlobalState.calledUsing}")
//            val oldScope = ctx.scope // always empty
//            println(s"XXXX5 newPlatform.oldScope: $oldScope ${oldScope.size}")
            val original = super.classPath
            val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
            val versionSortClassPath = ClassPathFactory.newClassPath(AbstractFile.getFile(versionSortJar))
//            val extJarsDir = "/home/mp/Projects/shiftleft/joern/extjars"
//            val extClassesDir = "/home/mp/Projects/shiftleft/joern/extclasses"
//            val directoryClassPath = ClassPathFactory.newClassPath(AbstractFile.getDirectory(extClassesDir))
//            val virtualDirectory = dotty.tools.io.VirtualDirectory("classes")
//            println(s"YYY1 new aggregate classpath; calledUsing=${HackyGlobalState.calledUsing}")
            val cpResult = if (HackyGlobalState.calledUsing) Seq(original, versionSortClassPath) else Seq(original)

//            val cp = new AggregateClassPath(cpResult)
//            println(s"YYY3 JavaPlatform.classpath called; calledUsing=${HackyGlobalState.calledUsing}")
//            HackyGlobalState.initialCp = cp
//            HackyGlobalState.initialCp
            new AggregateClassPath(cpResult) {
              override def list(inPackage: String) = {
                println("XXXX8 calling `classpath.list`")
//                if (HackyGlobalState.calledUsing) throw new AssertionError("boom")
//                else super.list(inPackage)
                super.list(inPackage)
              }
            }
          }
        }
        HackyGlobalState.jp = jp
        jp
      }
    }

    println(s"YYY2 ReplDriver.initCtx. calledUsing=${HackyGlobalState.calledUsing}")
//    throw new AssertionError("boom") // who's calling us?
    val ret = new Contexts.InitialContext(base, ctx.settings)
    ret
  }

  /** Run REPL with `state` until `:quit` command found
    * Main difference to the 'original': different greeting, trap Ctrl-c
   */
  override def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal(prompt)
    initializeRenderer()

    out.println(greeting)
//    println(s"XXXXX classloader: ${rendering.myClassLoader.findClass("foobar")}")

    /** Blockingly read a line, getting back a parse result */
    def readLine(state0: State): ParseResult = {
      val state =
        if (HackyGlobalState.calledUsing) {
          println("XXX7 fiddling with state")
          val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
          val oldCtx = state0.context
//          val newCp = s"${oldCtx.settings.classpath.value}$versionSortJar$classpathSeparator"
//          val ctx1 = oldCtx.fresh.setSetting(oldCtx.settings.classpath, newCp)
          val ctx1 = oldCtx.fresh.setSetting(oldCtx.settings.classpath, versionSortJar)
          val ctx2 = ctx1.setNewScope.setNewTyperState()
//          ctx2.typerState.fresh()
          val newState = state0.copy(context = ctx2)
//          ctx1.initialize()(using ctx1)
          newState
        } else state0
      val completer: Completer = { (_, line, candidates) =>
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.asJava)
      }
      given Context = state.context
      // TODO try, then change...
//      var ctx = state.context
      try {
        val line = terminal.readLine(completer)
//        val line = terminal.readLine(completer)(using ctx)
        // TODO extract, handle elsewhere
        if (line.startsWith("//> using")) {
          HackyGlobalState.calledUsing = true
//          val settings = Nil
//          rootCtx = initialCtx(settings)
//          if (rootCtx.settings.outputDir.isDefault(using rootCtx))
//            rootCtx = rootCtx.fresh
//              .setSetting(rootCtx.settings.outputDir, new dotty.tools.io.VirtualDirectory("<REPL compilation output>"))
//          compiler = new dotty.tools.repl.ReplCompiler
//          rendering = new dotty.tools.repl.Rendering(classLoader)
//          resetToInitial(Nil)
//          ctx = initCtx
//          ctx.fresh
//          rootCtx = ctx
//          rootCtx = initCtx

//          rootCtx = initialCtx(Nil)
//          val settings = args
//          val newRootCtx = initCtx//.fresh//.addMode(Mode.ReadPositions | Mode.Interactive)
//          newRootCtx.setSetting(newRootCtx.settings.YcookComments, true)
//          newRootCtx.setSetting(newRootCtx.settings.YreadComments, true)
//          setupRootCtx(this.settings ++ settings, newRootCtx)
//          rootCtx = setupRootCtx(this.args, newRootCtx) // this works but removes the state - drill deeper in here - can we save the state?
//          rootCtx = setup(settings, newRootCtx) match {
//            case Some((Nil, ictx)) => Contexts.inContext(ictx) {
              // still works... maybe the main thing happens in setup?
//              ictx.base.initialize() // test: inside `initialise, call `newPlatform`, but not `definitions.init`
//              ictx
//            }
//            case _ => ???
//          }

          // happens in `setup - drill in there...
//          rootCtx = setup(settings, newRootCtx).get._2

//          def setup(args: Array[String], rootCtx: Context): Context = {
//            val oldScope = rootCtx.scope // empty scope?
//            println(s"XXXX3 oldScope: $oldScope ${oldScope.size}") // always empty... look elsewhere
//
//            val ictx = rootCtx
//            val cmdDistill = command.distill(args, ictx.settings)(ictx.settingsState)(using ictx)
//            ictx
//          }
//          val newRootCtx = this.initCtx

            // no difference...
//            val freshContext: FreshContext = rootCtx.asInstanceOf[FreshContext]
//          freshContext.typerState.fresh()
//          freshContext.setDebug

          rootCtx = {
            val oldCtx: FreshContext = rootCtx.asInstanceOf[FreshContext]
            val baseCtx: ContextBase = new ContextBase {
              override val initialCtx: Context = oldCtx
              override def newPlatform(using Context): Platform = {
                new JavaPlatform {
                  override def classPath(using Context): ClassPath = {
                    println("new javaplatform -> classpath")
                    val original = super.classPath
                    val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
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
//            val newRootCtx = new Contexts.InitialContext(baseCtx, rootCtx.settings) // works, but loses history
//            command.distill(args, newRootCtx.settings)(newRootCtx.settingsState)(using newRootCtx)
//            newRootCtx

//            oldCtx.base.reset()
//            oldCtx
//              new FreshContext(baseCtx)
//              println(s"XXX5 cp=${oldCtx.settings.classpath.value}")
            val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
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
            ctx2
//            println(s"oldRootCtx class=${rootCtx.getClass}") // FreshContext
//            println(s"newRootCtx class=${newRootCtx.getClass}") //InitialContext
//            newRootCtx.freshOver(rootCtx) // no workie
//            rootCtx.freshOver(newRootCtx) // doesn't work either...
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
