package io.joern.solidity2cpg

import better.files.File
import io.joern.solidity2cpg.passes.AstCreationPass
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import java.io.{File => javaFile}
import java.io.PrintWriter
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
      val sourceFileNames         = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
      val dir                     = File.newTemporaryDirectory("solidity").deleteOnExit()
      val matches: Iterator[File] = sourceFile.glob("*.sol")
      matches.foreach(file => file.copyToDirectory(dir))
      val names: Array[String]           = new Array(1000)
      var pathOfParseFiles: List[String] = List()
      var counter                        = 0
      sourceFileNames.foreach(fName => {
        names(counter) = (fName.split("/")(fName.split("/").length - 1))
        ExternalCommand.run(s"surya parse -jc ${names(counter)}", dir.pathAsString) match {
          case Success(stdOut: Seq[String]) =>
            val path   = dir.pathAsString + s"/${names(counter).substring(0, (names(counter).length - 3))}" + "json"
            val writer = new PrintWriter(new javaFile(path))
            writer.write(stdOut.toString().substring(5, (stdOut.toString().length - 1)))
            writer.close()
            pathOfParseFiles = pathOfParseFiles :+ (path)
          case Failure(e) =>
            println(s"Could not parse Solidity source code at $sourceCodePath", e)

        }
//        ExternalCommand.run(s"npm -g","")
        counter += 1
      })
      (dir.toString(), pathOfParseFiles)
    } else {
      val dir                = File.newTemporaryDirectory("solidity").deleteOnExit()
      var list: List[String] = List()
      sourceFile.copyToDirectory(dir)
      val name = sourceFile.pathAsString.split("/")(sourceFile.pathAsString.split("/").length - 1)
      ExternalCommand.run(s"surya parse -jc ${name}", dir.pathAsString) match {
        case Success(stdOut: Seq[String]) =>
          val path   = dir.pathAsString + s"/${name.substring(0, name.length - 3) + "json"}"
          val writer = new PrintWriter(new javaFile(path))
          val output = stdOut.toString()
          writer.write(output.substring(5, (output.length - 1)))
          writer.close()
          list = list :+ (path)
        case Failure(e) =>
          println("Failure when executing Surya on :", e.printStackTrace())
      }
      (dir.pathAsString, list)
    }
  }

}
