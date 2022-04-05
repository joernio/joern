package io.joern.solidity2cpg

import better.files.File
import io.joern.solidity2cpg.passes.AstCreationPass
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg

import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.{Failure, Success}

object Solidity2Cpg {
  // TODO: Add to io.shiftleft.codepropertygraph.generated.Languages
  val language: String = "SOLIDITY"

  def apply(): Solidity2Cpg = new Solidity2Cpg()
}

class Solidity2Cpg {

  import Solidity2Cpg._

  val sourceFileExtensions      = Set(".sol")
  val suryaOutputFileExtensions = Set(".json")

  /** Create CPG for Solidity source code at `sourceCodePath` and store the CPG at `outputPath`. If `outputPath` is
    * `None`, the CPG is created in-memory.
    */
  def createCpg(sourceCodePath: String, outputPath: Option[String] = None): Cpg = {
    val cpg = newEmptyCpg(outputPath)

    new MetaDataPass(cpg, language).createAndApply()

    val (sourcesDir, sourceFileNames) = getSourcesFromDir(sourceCodePath)
    val astCreator                    = new AstCreationPass(sourcesDir, sourceFileNames, cpg)
    astCreator.createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
      .createAndApply()

    cpg
  }

  /** Surya reads Solidity source code and spits out JSON ASTs. If the given path is a single file we should copy it to
    * a temporary directory as to be able to pass a "sourcesDir" variable to [[AstCreationPass]]. Any .sol classes need
    * to be transformed to .json files (in-place and deleted on exit) whose paths are then returned as a list in the
    * second part of the tuple.
    */
  def getSourcesFromDir(sourceCodePath: String): (String, List[String]) = {
    val sourceFile = File(sourceCodePath)
    if (sourceFile.isDirectory) {
      val sourceFileNames = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
      // TODO: Convert sourceFiles to JSON by calling Surya. These will then be picked up in the next line
      sourceFileNames.foreach { fName => ExternalCommand.run(s"surya parse -j $fName", sourceCodePath) match {
        case Success(stdOut: Seq[String]) =>
          val jsonFiles = SourceFiles.determine(Set(sourceCodePath), suryaOutputFileExtensions)
          (sourceCodePath, jsonFiles)
        case Failure(e) =>
          println(s"Could not parse Solidity source code at $sourceCodePath", e)
        }
      }

      (sourceCodePath, null)
    } else {
      val dir = File.newTemporaryDirectory("solidity").deleteOnExit()
      sourceFile.copyToDirectory(dir).deleteOnExit()
      // TODO: Convert sourceFile to JSON by calling Surya. These will then be picked up in the next line
      ExternalCommand.run(s"surya parse -j ${sourceFile.pathAsString}", dir.pathAsString) match {
        case Success(stdOut: Seq[String]) =>
          val jsonFiles = SourceFiles.determine(Set(dir.pathAsString), suryaOutputFileExtensions)
          (dir.pathAsString, jsonFiles)
        case Failure(e) =>
          println("Failure when executing Surya on :", e)
          ("", List())
      }
//      val jsonFiles = SourceFiles.determine(Set(dir.pathAsString), suryaOutputFileExtensions)
//      (dir.pathAsString, jsonFiles)
    }
  }

}
