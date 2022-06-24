package io.joern.kotlin2cpg.compiler

import better.files.File
import io.joern.kotlin2cpg.DefaultContentRootJarPath
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Paths
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinToJVMBytecodeCompiler
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompilerAPITests extends AnyFreeSpec with Matchers {

  private val shouldPrintCompilerErrors = false // change to true when needed for debugging

  class ErrorCountMessageCollector extends MessageCollector {
    var errorCount = 0
    override def report(
      compilerMessageSeverity: CompilerMessageSeverity,
      s: String,
      compilerMessageSourceLocation: CompilerMessageSourceLocation
    ): Unit = {
      if (compilerMessageSeverity.isError) {
        if (shouldPrintCompilerErrors) println("message from compiler: " + s)
        errorCount += 1
      }
    }
    override def hasErrors: Boolean = errorCount != 0
    override def clear(): Unit      = {}
  }

  "KotlinCoreEnvironment generation on simple test code which calls external libraries" - {
    val projectDirPath          = ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/code/ktmin")
    val projectDependenciesPath = Paths.get(projectDirPath, "dependencies")

    "should not receive a compiler error message when the dependencies of the project have been provided" in {
      val defaultContentRootJarsDir = File(projectDependenciesPath)
      val contentRoots = defaultContentRootJarsDir.listRecursively
        .filter(_.pathAsString.endsWith("jar"))
        .map { f => DefaultContentRootJarPath(f.pathAsString, false) }
        .toSeq
      val messageCollector = new ErrorCountMessageCollector()
      val environment      = CompilerAPI.makeEnvironment(Seq(projectDirPath), contentRoots, Seq(), messageCollector)

      KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      messageCollector.hasErrors() shouldBe false
    }

    "should receive a compiler error message when the dependencies of the project have not been provided" in {
      val messageCollector = new ErrorCountMessageCollector()
      val environment      = CompilerAPI.makeEnvironment(Seq(projectDirPath), Seq(), Seq(), messageCollector)

      KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      messageCollector.hasErrors() shouldBe true
    }
  }
}
