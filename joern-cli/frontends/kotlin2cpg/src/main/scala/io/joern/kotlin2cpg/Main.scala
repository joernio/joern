package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.files.SourceFilesPicker
import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultNameGenerator, InferenceSourcesPicker}
import io.joern.kotlin2cpg.utils.PathUtils

import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig}
import io.shiftleft.utils.IOUtils

import better.files.File
import java.nio.file.{Files, Paths}
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scopt.OParser

case class InferenceJarPath(path: String, isResource: Boolean)

/** Command line configuration parameters
  */
final case class Config(
  inputPaths: Set[String] = Set.empty, // TODO: make this a singular
  outputPath: String = X2CpgConfig.defaultOutputPath,
  classpath: Set[String] = Set.empty
) extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config =
    copy(inputPaths = inputPaths + inputPath)

  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

/** Entry point for command line CPG creator
  */
object Main extends App {
  private val logger = LoggerFactory.getLogger(getClass)

  val parsingError = "KT2CPG_PARSING_ERROR"

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    import builder.opt
    OParser.sequence(
      programName("kotlin2cpg"),
      opt[String]("classpath")
        .unbounded()
        .text("directories to be searched for type resolution jars")
        .action((incl, c) => c.copy(classpath = c.classpath + incl))
    )
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) =>
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

        val filesWithKtExtension = SourceFiles.determine(Set(sourceDir), Set(".kt"))
        if (filesWithKtExtension.isEmpty) {
          println("The provided input directory does not contain files ending in '.kt' `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        logger.info(s"Starting CPG generation for input directory `$sourceDir`.")

        val dirsForSourcesToCompile = InferenceSourcesPicker.dirsForRoot(sourceDir)
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

        val inferenceJars =
          InferenceSourcesPicker.defaultInferenceJarPaths ++
            jarPathsFromClassPath.map { path => InferenceJarPath(path, false) }
        val messageCollector = new ErrorLoggingMessageCollector
        val environment = CompilerAPI.makeEnvironment(dirsForSourcesToCompile, inferenceJars, Seq(), messageCollector)
        val ktFiles     = environment.getSourceFiles.asScala
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

        val nameGenerator = new DefaultNameGenerator(environment)
        val cpg = new Kt2Cpg().createCpg(filesWithMeta, fileContentsAtPath, nameGenerator, Some(config.outputPath))
        cpg.close()
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    case None =>
      System.exit(1)
  }

}
