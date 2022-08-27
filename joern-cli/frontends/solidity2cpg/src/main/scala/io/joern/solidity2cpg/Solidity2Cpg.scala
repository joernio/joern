package io.joern.solidity2cpg

import better.files.File
import io.joern.solidity2cpg.passes.AstCreationPass
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

import java.io.{File => JFile}
import java.io.PrintWriter
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.{Failure, Success, Try, Using}

object Solidity2Cpg {
  // TODO: Add to io.shiftleft.codepropertygraph.generated.Languages
  val language: String = "SOLIDITY"

  def apply(): Solidity2Cpg = new Solidity2Cpg()
}

class Solidity2Cpg {

  import Solidity2Cpg._

  private val sourceFileExtensions      = Set(".sol")
  private val suryaOutputFileExtensions = Set(".json")

  private val logger = LoggerFactory.getLogger(classOf[Solidity2Cpg])

  /** Create CPG for Solidity source code at `sourceCodePath` and store the CPG at `outputPath`. If `outputPath` is
    * `None`, the CPG is created in-memory.
    */
  def createCpg(sourceCodePath: String, outputPath: Option[String] = None): Cpg = {
    val cpg = newEmptyCpg(outputPath)

    new MetaDataPass(cpg, language,"").createAndApply()

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
    import java.io.File.{separator => sep}
    val sourceFile = File(sourceCodePath)
    val dir        = File.newTemporaryDirectory("solidity").deleteOnExit()
    if (sourceFile.isDirectory) {
      SourceFiles
        .determine(Set(sourceCodePath), sourceFileExtensions)
        .map(File(_))
        .foreach(file => file.copyToDirectory(dir))
    } else {
      sourceFile.copyToDirectory(dir)
    }

    SourceFiles
      .determine(Set(dir.pathAsString), sourceFileExtensions)
      .map { fName => fName.stripPrefix(s"${dir.pathAsString}$sep") }
      .foreach(fileName => {
        ExternalCommand.run(s"surya parse -jlc $fileName", dir.pathAsString) match {
          case Success(stdOut: Seq[String]) =>
            val path = s"${dir.pathAsString}$sep${fileName.stripSuffix(".sol")}.json"
            Using.resource(new PrintWriter(new JFile(path))) { writer =>
              writer.write(stdOut.toString().substring(5, stdOut.toString().length - 1))
            }
          case Failure(e) =>
            logger.warn(s"Could not parse Solidity source code at $sourceCodePath", e)
        }
      })

    val outFileNames = SourceFiles.determine(Set(dir.pathAsString), suryaOutputFileExtensions)
    (dir.pathAsString, outFileNames)
  }


}
