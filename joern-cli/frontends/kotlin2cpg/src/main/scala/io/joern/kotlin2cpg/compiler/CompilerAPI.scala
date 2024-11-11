package io.joern.kotlin2cpg.compiler

import io.joern.kotlin2cpg.DefaultContentRootJarPath
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment}
import org.jetbrains.kotlin.cli.jvm.config.{JavaSourceRoot, JvmClasspathRoot}
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.config.{CommonConfigurationKeys, CompilerConfiguration, JVMConfigurationKeys}
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmProtoBufUtil
import org.slf4j.LoggerFactory

import java.io.File

object CompilerAPI {
  private val logger = LoggerFactory.getLogger(getClass)

  def makeEnvironment(
    forDirectories: Seq[String],
    javaSourceRoots: Seq[String],
    defaultContentRootJarPaths: Seq[DefaultContentRootJarPath] = List(),
    messageCollector: MessageCollector
  ): KotlinCoreEnvironment = {
    val config = new CompilerConfiguration()
    config.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
    forDirectories.foreach { p =>
      config.add(CLIConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(p, true, null))
    }

    val javaHome = File(System.getProperty("java.home"))
    config.put(JVMConfigurationKeys.JDK_HOME, javaHome)

    defaultContentRootJarPaths.foreach { path =>
      if (!path.isResource) {
        val f = new File(path.path)
        if (f.exists()) {
          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(f))
          logger.debug(s"Added dependency from path `${path.path}`.")
        } else {
          logger.warn(s"Path to dependency does not point to existing file `${path.path}`.")
        }
      } else {
        val resource = getClass.getClassLoader.getResource(path.path)
        if (resource != null) {
          val tempFile = File(resource.getFile)
          config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(tempFile))
          logger.debug(s"Added dependency from resources `${path.path}`.")
        } else {
          logger.warn(s"Path to default dependency does not point to existing resource `${path.path}`.")
        }
      }
    }

    javaSourceRoots.foreach { source =>
      val f = new File(source)
      config.add(CLIConfigurationKeys.CONTENT_ROOTS, new JavaSourceRoot(f, ""))
    }

    config.put(CommonConfigurationKeys.MODULE_NAME, JvmProtoBufUtil.DEFAULT_MODULE_NAME)

    val configFiles = EnvironmentConfigFiles.JVM_CONFIG_FILES
    val disposable  = Disposer.newDisposable()

    val environment = KotlinCoreEnvironment.createForProduction(disposable, config, configFiles)
    environment
  }
}

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
