package io.joern.console

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.classpath.{AggregateClassPath, ClassPathFactory}
import dotty.tools.dotc.config.{JavaPlatform, Platform}
import dotty.tools.dotc.core.Contexts
import dotty.tools.io.{AbstractFile, ClassPath}
import dotty.tools.repl.{CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}

import java.io.PrintStream
import org.jline.reader.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object HackyGlobalState {
//  var jp: JavaPlatform = null
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

  private val additionalDependencyJars: mutable.Set[String] = mutable.Set.empty

  def addDependency(jarPath: String): Unit = additionalDependencyJars.add(jarPath)

  override def initCtx: Context = {
    val ctx = super.initCtx
    ctx.fresh.setSetting(ctx.settings.VreplMaxPrintElements, maxPrintElements)
//    val base: ContextBase = ctx.base
//    val base: ContextBase = new ContextBase {
//      override def newPlatform(using Context): Platform = {
//        val jp = new JavaPlatform {
//          override def classPath(using Context): ClassPath = {
//            val original = super.classPath
//            val hppcJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/carrotsearch/hppc/0.7.1/hppc-0.7.1.jar"
//            val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
//            val jar = if (!HackyGlobalState.swap) hppcJar else versionSortJar
//            val initialCp = ClassPathFactory.newClassPath(AbstractFile.getFile(jar))
////            val extJarsDir = "/home/mp/Projects/shiftleft/joern/extjars"
////            val extClassesDir = "/home/mp/Projects/shiftleft/joern/extclasses"
////            val newCp = ClassPathFactory.newClassPath(AbstractFile.getDirectory(extClassesDir))
//            println(s"XXX1 new aggregate classpath created with $jar")
//            new AggregateClassPath(Seq(original, initialCp))
//          }
//        }
//        HackyGlobalState.jp = jp
//        jp
//      }
//    }

//    println("XXX2 ReplDriver.initCtx called")
//    new Contexts.InitialContext(base, ctx.settings)
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
          println("foo bar hacky state") // todo update ClassPath
          HackyGlobalState.classloader.clearAssertionStatus()
//          HackyGlobalState.swap = true
//          HackyGlobalState.jp.newClassLoader()
//          val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
//          val newCp = ClassPathFactory.newClassPath(AbstractFile.getFile(versionSortJar))
//          HackyGlobalState.jp.updateClassPath(
//            Map(HackyGlobalState.jp.classPath -> newCp)
//          )
          println("foo bar hacky state end") // todo update ClassPath
          // TODO impl properly
//          classL
        }
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
