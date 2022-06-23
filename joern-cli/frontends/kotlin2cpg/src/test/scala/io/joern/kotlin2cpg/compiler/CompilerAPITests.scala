package io.joern.kotlin2cpg.compiler

import better.files.{File}
import io.joern.kotlin2cpg.DefaultContentRootJarPath
import java.nio.file.Paths
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompilerAPITests extends AnyFreeSpec with Matchers {
  class ErrorCountMessageCollector extends MessageCollector {
    var errorCount = 0
    override def report(
      compilerMessageSeverity: CompilerMessageSeverity,
      s: String,
      compilerMessageSourceLocation: CompilerMessageSourceLocation
    ): Unit = {
      println("message from compiler: " + s)
      if (compilerMessageSeverity.isError) {
        errorCount += 1
      }
    }
    override def hasErrors: Boolean = errorCount != 0
    override def clear(): Unit      = {}
  }

  "KotlinCoreEnvironment generation on simple test code which calls external libraries" - {
    val uri                     = ClassLoader.getSystemResource("code/ktmin").toURI
    val projectDirPath          = Paths.get(uri).toString
    val projectDependenciesPath = Paths.get(projectDirPath, "dependencies")

    "should not receive a compiler error message when the dependencies of the project have been provided" in {
      val defaultContentRootJarsDir = File(projectDependenciesPath)
      val contentRoots = defaultContentRootJarsDir.listRecursively
        .filter(_.pathAsString.endsWith("jar"))
        .map { f => DefaultContentRootJarPath(f.pathAsString, false) }
        .toSeq
      val messageCollector = new ErrorCountMessageCollector()
      CompilerAPI.makeEnvironment(Seq(projectDirPath), contentRoots, Seq(), messageCollector)
      messageCollector.hasErrors() shouldBe false
    }

    "should receive a compiler error message when the dependencies of the project have not been provided" in {
      val messageCollector = new ErrorCountMessageCollector()
      CompilerAPI.makeEnvironment(Seq(projectDirPath), Seq(), Seq(), messageCollector)
      messageCollector.hasErrors() shouldBe false
    }
  }
}
