package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Try

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
      val (sourcesDir, sourceFileNames) = getSourcesFromDir(config.inputPath)
      if (sourceFileNames.isEmpty) {
        logger.error(s"no source files found in $sourcesDir")
      } else {
        logger.info(s"found ${sourceFileNames.size} source files")
      }

      val astCreator = new AstCreationPass(sourcesDir, sourceFileNames, config, cpg)
      astCreator.createAndApply()
      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
        .createAndApply()
    }
  }

  /** JavaParser requires that the input path is a directory and not a single source file. This is inconvenient for
    * small-scale testing, so if a single source file is created, copy it to a temp directory.
    */
  private def getSourcesFromDir(sourceCodePath: String): (String, List[String]) = {
    val sourceFile = File(sourceCodePath)
    if (sourceFile.isDirectory) {
      val sourceFileNames = SourceFiles.determine(sourceCodePath, sourceFileExtensions)
      (sourceCodePath, sourceFileNames)
    } else {
      val dir = File.newTemporaryDirectory("javasrc").deleteOnExit()
      sourceFile.copyToDirectory(dir).deleteOnExit()
      (dir.pathAsString, List(sourceFile.pathAsString))
    }
  }

}
