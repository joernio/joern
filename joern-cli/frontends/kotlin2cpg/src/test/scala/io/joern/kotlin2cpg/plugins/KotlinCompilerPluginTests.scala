package io.joern.kotlin2cpg.plugins

import better.files.File
import io.joern.kotlin2cpg.DefaultContentRootJarPath
import io.joern.kotlin2cpg.compiler.{CompilerAPI, CompilerPluginInfo}
import io.joern.kotlin2cpg.types.{ContentSourcesPicker, DefaultTypeInfoProvider}
import io.shiftleft.utils.ProjectRoot
import org.jetbrains.kotlin.allopen.{AllOpenComponentRegistrar, AllOpenConfigurationKeys}
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}
import org.scalatest.Ignore
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

@Ignore
class KotlinCompilerPluginTests extends AnyFreeSpec with Matchers {
  "Analysis of simple source code which requires the `kotlin-all-open` compiler plugin to compile" - {

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

    val defaultContentRootJarsDir =
      File(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/main/resources/jars/"))
    val defaultContentRootJarPaths =
      defaultContentRootJarsDir.list
        .filter(_.hasExtension)
        .filter(_.pathAsString.endsWith("jar"))
        .map { f =>
          DefaultContentRootJarPath(f.pathAsString, false)
        }
        .toSeq

    val sourceDir =
      ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/code/with_kotlin_all_open_plugin")
    "should receive a compiler error message if compiler plugins are not set up" in {
      val plugins          = Seq()
      val messageCollector = new ErrorCountMessageCollector()
      val environment =
        CompilerAPI.makeEnvironment(Seq(sourceDir), defaultContentRootJarPaths, plugins, messageCollector)
      val nameGenerator = new DefaultTypeInfoProvider(environment)
      nameGenerator.bindingContext should not be null
      messageCollector.hasErrors() shouldBe true
    }

    "should not receive a compiler error message if compiler plugins are set up" in {
      val registrarName = "all-open-registrar"
      val registrar     = new AllOpenComponentRegistrar()
      val pluginOptions =
        Map(AllOpenConfigurationKeys.INSTANCE.getANNOTATION -> "io.joern.placeholder.AllOpenAnnotation")
      val allOpenPluginInfo = CompilerPluginInfo(registrarName, registrar, pluginOptions)

      val plugins          = Seq(allOpenPluginInfo)
      val messageCollector = new ErrorCountMessageCollector()
      val environment =
        CompilerAPI.makeEnvironment(Seq(sourceDir), defaultContentRootJarPaths, plugins, messageCollector)
      val nameGenerator = new DefaultTypeInfoProvider(environment)
      nameGenerator.bindingContext should not be null
      messageCollector.hasErrors() shouldBe false
    }
  }
}
