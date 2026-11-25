package io.joern.rubysrc2cpg.passes

import flatgraph.DiffGraphApplier
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.report.{Category, ScanReport}
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

class AstCreationPass(cpg: Cpg, astCreators: List[AstCreator], report: ScanReport = new ScanReport())
    extends ForkJoinParallelCpgPass[AstCreator](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[AstCreator] = astCreators.toArray

  override def init(): Unit = {
    // The first entry will be the <empty> type, which is often found on fieldAccess nodes
    //   (which may be receivers to calls)
    val diffGraph = Cpg.newDiffGraphBuilder
    val emptyType =
      NewTypeDecl()
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(NamespaceTraversal.globalNamespaceName)
        .isExternal(true)
    val anyType =
      NewTypeDecl()
        .name(Defines.Any)
        .fullName(Defines.Any)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(NamespaceTraversal.globalNamespaceName)
        .isExternal(true)
    diffGraph.addNode(emptyType).addNode(anyType)
    DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    // the file was parsed when the AstCreator was created
    report.addReportInfo(astCreator.relativeFileName, astCreator.fileContent.linesIterator.size, parsed = true)
    val (gotCpg, duration) = TimeUtils.time {
      try {
        val ast = astCreator.createAst()
        diffGraph.absorb(ast)
        logger.debug(s"Processed '${astCreator.fileName}'")
        true
      } catch {
        case NonFatal(ex) =>
          report.warn(
            Category.FileProcessing,
            "Failed to process the file",
            Some(astCreator.relativeFileName),
            Some(ex),
            logger
          )
          false
      }
    }
    report.updateReport(astCreator.relativeFileName, gotCpg, duration)
  }
}
