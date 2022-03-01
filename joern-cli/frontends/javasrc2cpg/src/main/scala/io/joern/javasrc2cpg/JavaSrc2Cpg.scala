package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.X2Cpg.newEmptyCpg

import java.nio.file.Files
import scala.jdk.CollectionConverters.EnumerationHasAsScala

object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC

  def apply(): JavaSrc2Cpg = new JavaSrc2Cpg()
}

class JavaSrc2Cpg {

  import JavaSrc2Cpg._

  val sourceFileExtensions = Set(".java")

  /** Create CPG for Java source code at `inputPath` and store the CPG at `outputPath`. If `outputPath` is `None`, the
    * CPG is created in-memory.
    */
  def createCpg(inputPath: String, outputPath: Option[String] = None): Cpg = {
    val cpg             = newEmptyCpg(outputPath)
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool    = new IntervalKeyPool(100, 1000100)
    val methodKeyPool   = new IntervalKeyPool(first = 1000100, last = Long.MaxValue)

    new MetaDataPass(cpg, language, Some(metaDataKeyPool)).createAndApply()

    val (sourcesDir, sourceFileNames) = getSourcesFromDir(inputPath)
    val astCreator                    = new AstCreationPass(sourcesDir, sourceFileNames, cpg, methodKeyPool)
    astCreator.createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool))
      .createAndApply()

    cpg
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
