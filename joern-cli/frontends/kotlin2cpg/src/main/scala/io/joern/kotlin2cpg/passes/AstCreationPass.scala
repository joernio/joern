package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.kotlin2cpg.types.NameGenerator

import java.util.concurrent.ConcurrentHashMap
import org.slf4j.LoggerFactory

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class AstCreationPass(filesWithMeta: Iterable[KtFileWithMeta], nameGenerator: NameGenerator, cpg: Cpg)
    extends ConcurrentWriterCpgPass[String](cpg) {
  val global: Global = Global()
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
        diffGraph.absorb(new AstCreator(fm, nameGenerator, global).createAst())
        logger.debug("AST created for file at `" + filename + "`.")
      case None =>
        logger.info("Could not find file at `" + filename + "`.")
    }
  }

}
