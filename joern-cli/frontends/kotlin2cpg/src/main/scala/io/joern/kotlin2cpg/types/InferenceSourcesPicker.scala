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
