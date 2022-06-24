package io.joern.kotlin2cpg.compiler

import io.joern.kotlin2cpg.DefaultContentRootJarPath

import java.io.{File, FileOutputStream}
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment}
import org.jetbrains.kotlin.cli.jvm.config.JvmClasspathRoot
import org.jetbrains.kotlin.config.{CommonConfigurationKeys, CompilerConfiguration, CompilerConfigurationKey}
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmProtoBufUtil
import org.jetbrains.kotlin.com.intellij.mock.MockProject
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters.CollectionHasAsScala

case class CompilerPluginInfo(
  registrarName: String,
  registrar: ComponentRegistrar,
  configOptions: Map[CompilerConfigurationKey[java.util.List[String]], String]
) {}

object CompilerAPI {
  private val logger = LoggerFactory.getLogger(getClass)

  def makeEnvironment(
    forDirectories: Seq[String],
    defaultContentRootJarPaths: Seq[DefaultContentRootJarPath] = List(),
    compilerPlugins: Seq[CompilerPluginInfo] = Seq(),
    messageCollector: MessageCollector
  ): KotlinCoreEnvironment = {
    val config = new CompilerConfiguration()
    config.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
    forDirectories.foreach { p =>
      config.add(CLIConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(p, true))
    }

    defaultContentRootJarPaths.foreach { path =>
      if (!path.isResource) {
        val f = new File(path.path)
        if (f.exists()) {
          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(f))
          logger.debug("Added dependency from path `" + path.path + "`.")
        } else {
          logger.warn("Path to dependency does not point to existing file `" + path.path + "`.")
        }
      } else {
        val resourceStream = getClass.getClassLoader.getResourceAsStream(path.path)
        if (resourceStream != null) {
          val tempFile = File.createTempFile("kotlin2cpgDependencies", "")
          tempFile.deleteOnExit()
          val outStream = new FileOutputStream(tempFile)
          val buffer    = new Array[Byte](4096)

          while (resourceStream.available > 0) {
            val readBytes = resourceStream.read(buffer)
            outStream.write(buffer, 0, readBytes)
          }
          outStream.flush()
          outStream.close()

          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(tempFile))
          logger.debug("Added dependency from resources `" + path.path + "`.")
        } else {
          logger.warn("Path to default dependency does not point to existing resource `" + path.path + "`.")
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
  private val logger = LoggerFactory.getLogger(getClass)

  override def report(
    compilerMessageSeverity: CompilerMessageSeverity,
    s: String,
    compilerMessageSourceLocation: CompilerMessageSourceLocation
  ): Unit = {
    if (compilerMessageSeverity.isError) {
      logger.debug(s"Received error from Kotlin compiler: `$s`.")
    }
  }
  override def hasErrors: Boolean = false
  override def clear(): Unit      = {}
}
