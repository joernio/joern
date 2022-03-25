package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.joern.x2cpg.datastructures.Global

import org.slf4j.LoggerFactory

class AstCreationPass(filesWithMeta: Iterable[KtFileWithMeta], typeInfoProvider: TypeInfoProvider, cpg: Cpg)
    extends ConcurrentWriterCpgPass[String](cpg) {
  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] =
    filesWithMeta.map { ktFileWithMeta => ktFileWithMeta.f.getVirtualFilePath }.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val fileWithMeta = filesWithMeta
      .filter { ktFileWithMeta =>
        ktFileWithMeta.f.getVirtualFilePath == filename
      }
      .toList
      .headOption
    fileWithMeta match {
      case Some(fm) =>
        diffGraph.absorb(new AstCreator(fm, typeInfoProvider, global).createAst())
        logger.debug("AST created for file at `" + filename + "`.")
      case None =>
        logger.info("Could not find file at `" + filename + "`.")
    }
  }

}
