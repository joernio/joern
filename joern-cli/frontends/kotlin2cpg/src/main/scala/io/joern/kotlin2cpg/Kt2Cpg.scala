package io.joern.kotlin2cpg

import org.jetbrains.kotlin.psi.KtFile

import scala.jdk.CollectionConverters.EnumerationHasAsScala
import io.joern.kotlin2cpg.passes.{AstCreationPass, ConfigPass}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.kotlin2cpg.types.NameGenerator
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg

object Kt2Cpg {
  val language = "KOTLIN"

  case class InputPair(content: String, fileName: String)
  type InputProvider = () => InputPair
}

case class KtFileWithMeta(f: KtFile, relativizedPath: String, filename: String)
case class FileContentAtPath(content: String, relativizedPath: String, filename: String)

class Kt2Cpg {

  import Kt2Cpg._

  def createCpg(
    filesWithMeta: Iterable[KtFileWithMeta],
    fileContentsAtPath: Iterable[FileContentAtPath],
    nameGenerator: NameGenerator,
    outputPath: Option[String] = None
  ): Cpg = {
    val cpg = newEmptyCpg(outputPath)

    new MetaDataPass(cpg, language).createAndApply()

    val astCreator =
      new AstCreationPass(filesWithMeta, nameGenerator, cpg)
    astCreator.createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
      .createAndApply()

    val configCreator = new ConfigPass(fileContentsAtPath, cpg)
    configCreator.createAndApply()

    cpg
  }

}
