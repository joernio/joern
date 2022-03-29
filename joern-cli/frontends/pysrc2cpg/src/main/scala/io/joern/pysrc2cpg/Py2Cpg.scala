package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.parallel

object Py2Cpg {
  case class InputPair(content: String, absFileName: String, relFileName: String)
  type InputProvider = () => InputPair
}

/** Entry point for general cpg generation from python code.
  * @param inputProviders
  *   Set of functions which provide InputPairs. The functions must be safe to call from different threads.
  * @param outputCpg
  *   Empty target cpg which will be populated.
  */
class Py2Cpg(inputProviders: Iterable[Py2Cpg.InputProvider], outputCpg: Cpg) {
  private val diffGraph   = new DiffGraphBuilder()
  private val nodeBuilder = new NodeBuilder(diffGraph)
  private val edgeBuilder = new EdgeBuilder(diffGraph)

  def buildCpg(): Unit = {
    nodeBuilder.metaNode(Languages.PYTHONSRC, version = "")
    val globalNamespaceBlock =
      nodeBuilder.namespaceBlockNode(Constants.GLOBAL_NAMESPACE, Constants.GLOBAL_NAMESPACE, "N/A")
    nodeBuilder.typeNode(Constants.ANY, Constants.ANY)
    val anyTypeDecl = nodeBuilder.typeDeclNode(Constants.ANY, Constants.ANY, "N/A", Nil, LineAndColumn(1, 1, 1, 1))
    edgeBuilder.astEdge(anyTypeDecl, globalNamespaceBlock, 0)
    BatchedUpdate.applyDiff(outputCpg.graph, diffGraph)
    new CodeToCpg(outputCpg, inputProviders).createAndApply()
  }
}
