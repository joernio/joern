package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgConfig}
import io.joern.x2cpg.X2Cpg.{newEmptyCpg, withErrorsToConsole}

import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Try

object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC

  def apply(): JavaSrc2Cpg = new JavaSrc2Cpg()
}

trait X2CpgFrontend[T <: X2CpgConfig[_]] {
  def run(config: T): Unit
}

class JavaSrc2Cpg extends X2CpgFrontend[Config] {

  import JavaSrc2Cpg._

  val sourceFileExtensions = Set(".java")

  /** Create CPG according to given configuration, printing errors to the console if they occur. The CPG is not
    * returned.
    */
  def run(config: Config): Unit = {
    withErrorsToConsole(config) { _ =>
      if (config.inputPaths.size == 1) {
        createCpg(config.inputPaths.head, Some(config.outputPath))
      } else {
        throw new RuntimeException("This frontend requires exactly one input path")
      }
    }
  }

  /** Create CPG for Java source code at `sourceCodePath` and store the CPG at `outputPath`. If `outputPath` is `None`,
    * the CPG is created in-memory.
    */
  def createCpg(sourceCodePath: String, outputPath: Option[String] = None): Try[Cpg] = {
    Try {
      val cpg = newEmptyCpg(outputPath)
      new MetaDataPass(cpg, language).createAndApply()

      val (sourcesDir, sourceFileNames) = getSourcesFromDir(sourceCodePath)
      val astCreator                    = new AstCreationPass(sourcesDir, sourceFileNames, cpg)
      astCreator.createAndApply()

      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
        .createAndApply()

      cpg
    }
  }

  /** JavaParser requires that the input path is a directory and not a single source file. This is inconvenient for
    * small-scale testing, so if a single source file is created, copy it to a temp directory.
    */
  private def getSourcesFromDir(sourceCodePath: String): (String, List[String]) = {
    val sourceFile = File(sourceCodePath)
    if (sourceFile.isDirectory) {
      val sourceFileNames = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
      (sourceCodePath, sourceFileNames)
    } else {
      val dir = File.newTemporaryDirectory("javasrc").deleteOnExit()
      sourceFile.copyToDirectory(dir).deleteOnExit()
      (dir.pathAsString, List(sourceFile.pathAsString))
    }
  }

}
