package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.InferenceJarPath
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment}
import org.jetbrains.kotlin.cli.jvm.config.JvmClasspathRoot
import org.jetbrains.kotlin.config.{CommonConfigurationKeys, CompilerConfiguration, CompilerConfigurationKey}
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmProtoBufUtil
import org.slf4j.LoggerFactory

import java.io.{File, FileOutputStream}
import better.files.{File => BFile}
import org.jetbrains.kotlin.com.intellij.mock.MockProject
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object InferenceSourcesPicker {

  // In the following directory structure:
  //  ____________________
  //  | dir1
  //  |   -> build.gradle.kts
  //  |   -> dir2
  //  |      -> build.gradle.kts
  //  |      -> dir3
  //  |        -> source1.kt
  //  |        -> source2.kt
  //  |-------------------
  //  The list of paths which are acceptable for the current version of the Kotlin compiler API is:
  //  `Seq("dir1/dir2/dir3")` and nothing else.

  def dirsForRoot(rootDir: String): Seq[String] = {
    val dir = BFile(rootDir)

    // for an initial version, we only do very basic selection logic for `rootDir` without subdirs
    val hasSubDirs = dir.list.exists(_.isDirectory)
    if (!hasSubDirs) {
      return Seq(rootDir)
    }

    dir.listRecursively
      .filter(_.isDirectory)
      .flatMap { f =>
        val dirsPicked =
          f.list
            .filter(_.isDirectory)
            .filterNot { d =>
              d.listRecursively
                .filter(_.hasExtension)
                .exists(_.pathAsString.endsWith(".kts"))
            }
            .toList
            .map(_.pathAsString)
        if (dirsPicked.nonEmpty) {
          Some(dirsPicked)
        } else {
          None
        }
      }
      .flatten
      .toList
  }
}

case class CompilerPluginInfo(
  registrarName: String,
  registrar: ComponentRegistrar,
  configOptions: Map[CompilerConfigurationKey[java.util.List[String]], String]
) {}

object CompilerAPI {
  private val logger = LoggerFactory.getLogger(getClass)

  def makeEnvironment(
    forDirectories: Seq[String],
    inferenceJarPaths: Seq[InferenceJarPath] = List(),
    compilerPlugins: Seq[CompilerPluginInfo] = Seq(),
    messageCollector: MessageCollector
  ): KotlinCoreEnvironment = {
    val config = new CompilerConfiguration()
    config.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
    forDirectories.foreach { p =>
      config.add(CLIConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(p, true))
    }

    inferenceJarPaths.foreach { path =>
      if (!path.isResource) {
        val f = new File(path.path)
        if (f.exists()) {
          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(f))
          logger.debug("Added inference jar from path `" + path.path + "`.")
        } else {
          logger.warn("Path to inference jar does not point to existing file `" + path.path + "`.")
        }
      } else {
        val resourceStream = getClass.getClassLoader.getResourceAsStream(path.path)
        if (resourceStream != null) {
          val tempFile = File.createTempFile("inference", "", new File("./"))
          tempFile.deleteOnExit()
          val outStream = new FileOutputStream(tempFile)

          val bytes =
            LazyList.continually(resourceStream.read).takeWhile(_ != -1).map(_.toByte).toArray
          outStream.write(bytes)
          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(tempFile))
          logger.debug("Added inference jar from resources `" + path.path + "`.")
        } else {
          logger.warn("Path to inference jar does not point to existing resource `" + path.path + "`.")
        }
      }
    }
    config.put(CommonConfigurationKeys.MODULE_NAME, JvmProtoBufUtil.DEFAULT_MODULE_NAME)

    val configFiles = EnvironmentConfigFiles.JVM_CONFIG_FILES
    val disposable  = Disposer.newDisposable()

    val registrarKeys =
      compilerPlugins.map { plugin =>
        plugin.registrarName -> CompilerConfigurationKey.create[java.util.List[ComponentRegistrar]](
          plugin.registrarName
        )
      }.toMap
    compilerPlugins.foreach { plugin =>
      plugin.configOptions.foreach { case (k, v) =>
        config.add(k, v)
      }
      config.add(registrarKeys(plugin.registrarName), plugin.registrar)
    }
    val environment = KotlinCoreEnvironment.createForProduction(disposable, config, configFiles)
    compilerPlugins.foreach { plugin =>
      config.getList(registrarKeys(plugin.registrarName)).asScala.foreach { r =>
        r.registerProjectComponents(environment.getProject.asInstanceOf[MockProject], config)
      }
    }

    environment
  }
}

class CompilerAPI {}

class ErrorLoggingMessageCollector extends MessageCollector {
  override def report(
    compilerMessageSeverity: CompilerMessageSeverity,
    s: String,
    compilerMessageSourceLocation: CompilerMessageSourceLocation
  ): Unit = {}
  override def hasErrors: Boolean = false
  override def clear(): Unit      = {}
}
