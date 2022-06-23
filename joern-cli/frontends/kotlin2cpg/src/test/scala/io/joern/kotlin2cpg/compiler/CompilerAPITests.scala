package io.joern.kotlin2cpg.compiler

import better.files.{File, Resource}
import io.joern.kotlin2cpg.DefaultContentRootJarPath
import io.joern.kotlin2cpg.types.DefaultTypeInfoProvider
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
    val projectDirUrlOpt = Resource.url("code/ktmin")
    projectDirUrlOpt should not be empty
    val projectDir                   = projectDirUrlOpt.get
    val projectDependenciesDirUrlOpt = Resource.url("code/ktmin/dependencies")
    projectDependenciesDirUrlOpt should not be empty
    val projectDependenciesDirUrl = projectDependenciesDirUrlOpt.get

    "should not receive a compiler error message when the dependencies of the project have been provided" in {
      val defaultContentRootJarsDir = File(projectDependenciesDirUrl.getPath)
      val contentRoots = defaultContentRootJarsDir.listRecursively
        .filter(_.pathAsString.endsWith("jar"))
        .map { f => DefaultContentRootJarPath(f.pathAsString, false) }
        .toSeq
      val messageCollector = new ErrorCountMessageCollector()
      CompilerAPI.makeEnvironment(Seq(projectDir.getPath), contentRoots, Seq(), messageCollector)
      messageCollector.hasErrors() shouldBe false
    }

    "should receive a compiler error message when the dependencies of the project have not been provided" in {
      val messageCollector = new ErrorCountMessageCollector()
      CompilerAPI.makeEnvironment(Seq(projectDir.getPath), Seq(), Seq(), messageCollector)
      messageCollector.hasErrors() shouldBe false
    }
  }
}
