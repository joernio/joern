package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.{AstCreationPass, ConfigFileCreationPass}
import io.joern.javasrc2cpg.util.Delombok
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

case class SourceDirectoryInfo(typeSolverSourceDir: String, sourceFiles: List[String])
object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC

  def apply(): JavaSrc2Cpg = new JavaSrc2Cpg()
}

class JavaSrc2Cpg extends X2CpgFrontend[Config] {
  import JavaSrc2Cpg._
  private val logger = LoggerFactory.getLogger(getClass)

  val sourceFileExtensions = Set(".java")

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

      val astCreator =
        new AstCreationPass(sourcesInfo.typeSolverSourceDir, sourcesInfo.sourceFiles, config, cpg, dependencies)
      astCreator.createAndApply()
      new ConfigFileCreationPass(config.inputPath, cpg).createAndApply()
      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
        .createAndApply()
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

  /** JavaParser requires that the input path is a directory and not a single source file. This is inconvenient for
    * small-scale testing, so if a single source file is created, copy it to a temp directory.
    */
  private def getSourcesFromDir(config: Config, hasLombokDependency: Boolean): SourceDirectoryInfo = {

    if (hasLombokDependency) {
      logger.info(s"Analysing delomboked code as lombok dependency was found.")
    }
    val shouldAnalyzeDelombokedCode = config.delombokFullAnalysis || hasLombokDependency
    val inputPathAsFile             = File(config.inputPath)

    val originalSourcesDir = if (inputPathAsFile.isDirectory) {
      config.inputPath
    } else {
      val dir = File.newTemporaryDirectory("javasrc").deleteOnExit()
      inputPathAsFile.copyToDirectory(dir).deleteOnExit()
      dir.pathAsString
    }

    val delombokSourcesDir = Option.when(shouldAnalyzeDelombokedCode || config.delombokTypesOnly) {
      Delombok.run(originalSourcesDir, config.delombokJavaHome)
    }

    val analysisSourceFilePath =
      if (shouldAnalyzeDelombokedCode)
        delombokSourcesDir.get
      else
        originalSourcesDir

    val typeSourcesPath =
      if (shouldAnalyzeDelombokedCode || config.delombokTypesOnly)
        delombokSourcesDir.get
      else
        originalSourcesDir

    val sourceFileNames = SourceFiles.determine(analysisSourceFilePath, sourceFileExtensions)

    SourceDirectoryInfo(typeSourcesPath, sourceFileNames)
  }

}
