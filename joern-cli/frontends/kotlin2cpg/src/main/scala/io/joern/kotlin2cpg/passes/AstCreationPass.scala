package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.ast.AstCreator
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class AstCreationPass(filesWithMeta: Iterable[KtFileWithMeta], typeInfoProvider: TypeInfoProvider, cpg: Cpg)(implicit
  withSchemaValidation: ValidationMode
) extends ConcurrentWriterCpgPass[KtFileWithMeta](cpg) {
  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[KtFileWithMeta] =
    filesWithMeta.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileWithMeta: KtFileWithMeta): Unit = {
    diffGraph.absorb(new AstCreator(fileWithMeta, typeInfoProvider, global).createAst())
    logger.debug(s"AST created for file at `${fileWithMeta.f.getVirtualFilePath}`.")
  }

}
