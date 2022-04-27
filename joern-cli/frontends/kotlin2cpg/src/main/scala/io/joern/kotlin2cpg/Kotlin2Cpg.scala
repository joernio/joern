package io.joern.kotlin2cpg

import better.files.File
import io.joern.kotlin2cpg.files.SourceFilesPicker
import org.jetbrains.kotlin.psi.KtFile

import scala.jdk.CollectionConverters.{CollectionHasAsScala, EnumerationHasAsScala}
import io.joern.kotlin2cpg.passes.{AstCreationPass, ConfigPass}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.kotlin2cpg.types.{
  CompilerAPI,
  ContentSourcesPicker,
  DefaultTypeInfoProvider,
  ErrorLoggingMessageCollector
}
import io.joern.kotlin2cpg.utils.PathUtils
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.utils.dependency.{DependencyResolver, GradleDependencies}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import java.nio.file.{Files, Paths}
import scala.util.Try

object Kotlin2Cpg {
  val language = "KOTLIN"
  case class InputPair(content: String, fileName: String)
  type InputProvider = () => InputPair
}

case class KtFileWithMeta(f: KtFile, relativizedPath: String, filename: String)
case class FileContentAtPath(content: String, relativizedPath: String, filename: String)

class Kotlin2Cpg extends X2CpgFrontend[Config] {
  private val logger = LoggerFactory.getLogger(getClass)
  val parsingError   = "KOTLIN2CPG_PARSING_ERROR"

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      if (config.inputPaths.size == 1) {
        val sourceDir = config.inputPaths.head
        if (!Files.exists(Paths.get(sourceDir))) {
          println("The input path provided does not exist `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        if (!Files.isDirectory(Paths.get(sourceDir))) {
          println("The input path provided is not a directory `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        val copiedRuntimeLibsJarPaths =
          if (config.copyRuntimeLibs) {
            val paths = DependencyResolver.getDependencies(Paths.get(sourceDir))
            logger.info(s"Using ${paths.size} runtime libs from the build system found at the input path.")
            paths
          } else {
            logger.info(s"Not using any runtime libs from the build system found at the input path.")
            Seq()
          }

        val filesWithKtExtension = SourceFiles.determine(Set(sourceDir), Set(".kt"))
        if (filesWithKtExtension.isEmpty) {
          println("The provided input directory does not contain files ending in '.kt' `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        logger.info(s"Starting CPG generation for input directory `$sourceDir`.")

        val dirsForSourcesToCompile = ContentSourcesPicker.dirsForRoot(sourceDir)
        val jarPathsFromClassPath =
          config.classpath.foldLeft(Seq[String]())((acc, classpathEntry) => {
            val f = File(classpathEntry)
            val files =
              if (f.isDirectory) f.list.filter(_.extension.getOrElse("") == ".jar").map(_.toString)
              else Seq()
            acc ++ files
          })
        if (config.classpath.nonEmpty) {
          if (jarPathsFromClassPath.nonEmpty) {
            logger.info(s"Found ${jarPathsFromClassPath.size} jars in the specified classpath.")
          } else {
            logger.warn("No jars found in the specified classpath.")
          }
        }

        val plugins = Seq()
        val stdlibJars =
          if (config.withStdlibJarsInClassPath) {
            ContentSourcesPicker.defaultKotlinStdlibContentRootJarPaths
          } else {
            Seq()
          }
        val androidJars =
          if (config.withAndroidJarsInClassPath) {
            ContentSourcesPicker.defaultAndroidContentRootJarPaths
          } else {
            Seq()
          }
        val defaultContentRootJars =
          androidJars ++ stdlibJars ++
            jarPathsFromClassPath.map { path => DefaultContentRootJarPath(path, false) } ++
            copiedRuntimeLibsJarPaths.map { path =>
              DefaultContentRootJarPath(path, false)
            }
        val messageCollector = new ErrorLoggingMessageCollector
        val environment =
          CompilerAPI.makeEnvironment(dirsForSourcesToCompile, defaultContentRootJars, plugins, messageCollector)

        val ktFiles = environment.getSourceFiles.asScala
        val filesWithMeta =
          ktFiles
            .flatMap { f =>
              try {
                val relPath = PathUtils.relativize(sourceDir, f.getVirtualFilePath)
                Some(f, relPath)
              } catch {
                case _: Throwable => None
              }
            }
            .map { fwp =>
              KtFileWithMeta(fwp._1, fwp._2, fwp._1.getVirtualFilePath)
            }
            .filterNot { fwp =>
              // TODO: add test for this type of filtering
              // TODO: support Windows paths
              val willFilter = SourceFilesPicker.shouldFilter(fwp.relativizedPath)
              if (willFilter) {
                logger.debug("Filtered file at `" + fwp.f.getVirtualFilePath + "`.")
              }
              willFilter
            }

        val fileContentsAtPath =
          SourceFilesPicker
            .configFiles(sourceDir)
            .flatMap { fileName =>
              try {
                val relPath = PathUtils.relativize(sourceDir, fileName)
                Some(fileName, relPath)
              } catch {
                case _: Throwable => None
              }
            }
            .map { fnm =>
              val fileContents =
                try {
                  IOUtils.readLinesInFile(Paths.get(fnm._1)).mkString("\n")
                } catch {
                  case t: Throwable => parsingError + "\n" + t.toString
                }
              FileContentAtPath(fileContents, fnm._2, fnm._1)
            }

        val typeInfoProvider = new DefaultTypeInfoProvider(environment)

        new MetaDataPass(cpg, Languages.KOTLIN).createAndApply()
        val astCreator = new AstCreationPass(filesWithMeta, typeInfoProvider, cpg)
        astCreator.createAndApply()
        new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
        val configCreator = new ConfigPass(fileContentsAtPath, cpg)
        configCreator.createAndApply()

        val hasAtLeastOneMethodNode = cpg.method.take(1).nonEmpty
        if (!hasAtLeastOneMethodNode) {
          logger.warn("Resulting CPG does not contain any METHOD nodes.")
        }
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    }
  }
}
