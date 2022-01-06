package io.joern.kotlin2cpg

import org.jetbrains.kotlin.psi.KtFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala

import io.joern.kotlin2cpg.passes.{AstCreationPass, ConfigPass}
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg

object Kt2Cpg {
  val language = "KOTLIN"

  case class InputPair(content: String, fileName: String)
  type InputProvider = () => InputPair
}

case class KtFileWithMeta(f: KtFile, relativizedPath: String, filename: String)

class Kt2Cpg {

  import Kt2Cpg._

  def createCpg(
      filesWithMeta: Iterable[KtFileWithMeta],
      otherInputProviders: Iterable[InputProvider],
      typeInfoProvider: TypeInfoProvider,
      outputPath: Option[String] = None
  ): Cpg = {
    val cpg = newEmptyCpg(outputPath)
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool = new IntervalKeyPool(100, 1000100)
    val configKeyPool = new IntervalKeyPool(1000100, 2000100)
    val methodKeyPool = new IntervalKeyPool(first = 2000100, last = Long.MaxValue)

    new MetaDataPass(cpg, language, Some(metaDataKeyPool)).createAndApply()

    val astCreator =
      new AstCreationPass(filesWithMeta, typeInfoProvider, cpg, methodKeyPool)
    astCreator.createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool))
      .createAndApply()

    val configCreator = new ConfigPass(otherInputProviders, cpg, configKeyPool)
    configCreator.createAndApply()

    cpg
  }

}
