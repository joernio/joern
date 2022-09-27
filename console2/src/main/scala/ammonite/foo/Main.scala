package ammonite.foo

import dotty.tools.MainGenericCompiler.classpathSeparator
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, ContextState, FreshContext, NoContext, ctx}
import dotty.tools.dotc.ast.{Positioned, tpd, untpd}
import dotty.tools.dotc.classpath.{AggregateClassPath, ClassPathFactory}
import dotty.tools.dotc.config.{Feature, JavaPlatform, Platform}
import dotty.tools.dotc.core.{Contexts, MacroClassLoader, Mode, TyperState}
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation}
import dotty.tools.repl.{AbstractFileClassLoader, CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}

import java.io.PrintStream
import org.jline.reader.*

import java.net.URL
import javax.naming.InitialContext
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Main {
  def main(args: Array[String]): Unit = {
     val printer = Printer(
       outStream = System.out,
       errStream = System.err,
       resultStream = System.out,
       warning = msg => println(s"Xwarn: $msg"),
       error = msg => println(s"Xerror: $msg"),
       info = msg => println(s"Xinfo: $msg")
     )
     val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
     val versionSortJarUrl = new URL(s"file:$versionSortJar")

     val initialClassLoader = getClass.getClassLoader
     val initialClassPath = Classpath.classpath(initialClassLoader, None)

      // TODO implement completer
      val completer: Completer = { (_, line, candidates) =>
        //      val comps = completions(line.cursor, line.line, state)
        //      candidates.addAll(comps.asJava)
      }

      val compiler0 = new Compiler(
        dynamicClassPath = dotty.tools.io.AbstractFile.getDirectory("."),
        initialClassPath = initialClassPath,
        classPath = initialClassPath,
        whiteList = Set.empty
      )
      given Context = compiler0.initialCtx // TODO always get latest context for completions...

      val prompt = "joern2> "
      val terminal = new JLineTerminal(prompt)

      val line = terminal.readLine(completer)
      println(s"read: $line")

  //   val compiler1 = new Compiler(
  //     compiler0.dynamicClassPath,
  //     compiler0.initialClassPath,
  //     compiler0.classPath :+ versionSortJarUrl,
  //     compiler0.whiteList)
  //   val cmd0Src = """val foo = 42"""
  //   val compileResult0 = compiler0.compile(
  //     src = cmd0Src.getBytes("UTF-8"),
  //     printer,
  //     importsLen = 0,
  //     userCodeNestingLevel = 0,
  //     fileName = "cmd0.sc"
  //   )
  //   println(compileResult0.get) // foo defined successfully

  //   val cmd1Src =
  //     """val bar = foo
  //       |val baz = versionsort.VersionHelper.compare("1.0", "0.9")
  //       |""".stripMargin
  //   val compileResult1 = compiler1.compile(
  //     src = cmd1Src.getBytes("UTF-8"),
  //     printer,
  //     importsLen = 0,
  //     userCodeNestingLevel = 0,
  //     fileName = "cmd1.sc"
  //   )
  //   println(compileResult1.get) // bar|baz defined successfully :)

  }

}
