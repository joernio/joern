package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.{AstCreationPass, ConfigFileCreationPass, TypeInferencePass}
import io.joern.javasrc2cpg.util.{Delombok, SourceRootFinder}
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.utils.dependency.DependencyResolver
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Try

case class SourceDirectoryInfo(typeSolverSourceDirs: List[String], sourceFiles: List[SourceFileInfo])
case class SourceFileInfo(analysisFileName: String, originalFileName: String)
object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC

  def apply(): JavaSrc2Cpg = new JavaSrc2Cpg()
}

class JavaSrc2Cpg extends X2CpgFrontend[Config] {
  import JavaSrc2Cpg._
  private val logger = LoggerFactory.getLogger(getClass)

  val sourceFileExtensions: Set[String] = Set(".java")

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()
      val dependencies        = getDependencyList(config)
      val hasLombokDependency = dependencies.exists(_.contains("lombok"))
      val sourcesInfo         = getSourcesFromDir(config, hasLombokDependency)
      if (sourcesInfo.sourceFiles.isEmpty) {
        logger.error(s"no source files found at path ${config.inputPath}")
      } else {
        logger.info(s"found ${sourcesInfo.sourceFiles.size} source files")
      }

      val astCreator = new AstCreationPass(sourcesInfo, config, cpg, dependencies)
      astCreator.createAndApply()
      new ConfigFileCreationPass(config.inputPath, cpg).createAndApply()
      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
        .createAndApply()
      new TypeInferencePass(cpg).createAndApply()
    }
  }

  private def getDependencyList(config: Config): Seq[String] = {
    val codeDir = config.inputPath
    if (config.fetchDependencies) {
      DependencyResolver.getDependencies(Paths.get(codeDir)) match {
        case Some(deps) => deps.toSeq
        case None =>
          logger.warn(s"Could not fetch dependencies for project at path $codeDir")
          Seq()
      }
    } else {
      logger.info("dependency resolving disabled")
      Seq()
    }
  }

  private def getDelombokMode(config: Config): DelombokMode = {
    config.delombokMode.map(_.toLowerCase) match {
      case None                 => DelombokMode.Default
      case Some("no-delombok")  => DelombokMode.NoDelombok
      case Some("default")      => DelombokMode.Default
      case Some("types-only")   => DelombokMode.TypesOnly
      case Some("run-delombok") => DelombokMode.RunDelombok
      case Some(value) =>
        logger.warn(s"Found unrecognised delombok mode `$value`. Using default instead.")
        DelombokMode.Default
    }
  }

  /** JavaParser requires that the input path is a directory and not a single source file. This is inconvenient for
    * small-scale testing, so if a single source file is created, copy it to a temp directory.
    */
  private def getSourcesFromDir(config: Config, hasLombokDependency: Boolean): SourceDirectoryInfo = {
    val delombokMode = getDelombokMode(config)
    val runDelombok = delombokMode match {
      case DelombokMode.NoDelombok => false
      case DelombokMode.Default =>
        if (hasLombokDependency) {
          logger.info(s"Analysing delomboked code as lombok dependency was found.")
        }
        hasLombokDependency
      case DelombokMode.RunDelombok => true
      case DelombokMode.TypesOnly   => true
    }

    // val inputPathAsFile = File(config.inputPath)
    val sourceInfoTuples = SourceRootFinder.getSourceRoots(config.inputPath).map { inputPath =>
      val inputPathAsFile = File(inputPath)
      val originalSourcesDir = if (inputPathAsFile.isDirectory) {
        inputPath
      } else {
        val dir = File.newTemporaryDirectory("javasrc").deleteOnExit()
        inputPathAsFile.copyToDirectory(dir).deleteOnExit()
        dir.pathAsString
      }

      val delombokSourcesDir = Option.when(runDelombok) {
        Delombok.run(originalSourcesDir, config.delombokJavaHome)
      }

      val analysisSourceFilePath =
        if (runDelombok && (delombokMode != DelombokMode.TypesOnly))
          delombokSourcesDir.get
        else
          originalSourcesDir

      val typeSourcesPath = delombokSourcesDir.getOrElse(originalSourcesDir)

      val sourceFileNames = SourceFiles.determine(analysisSourceFilePath, sourceFileExtensions)
      val sourceFileInfo = delombokSourcesDir match {
        case Some(delombokSourcesDir) =>
          sourceFileNames.map { fileName =>
            // Directory structure remains unchanged.
            val originalFileName = fileName.replace(delombokSourcesDir, originalSourcesDir)
            SourceFileInfo(fileName, originalFileName)
          }

        case None => sourceFileNames.map { filename => SourceFileInfo(filename, filename) }
      }

      (typeSourcesPath, sourceFileInfo)
    // SourceDirectoryInfo(typeSourcesPath, sourceFileInfo)
    }

    val typesPaths   = sourceInfoTuples.map(_._1)
    val sourcesPaths = sourceInfoTuples.flatMap(_._2)
    SourceDirectoryInfo(typesPaths, sourcesPaths)
  }

}
