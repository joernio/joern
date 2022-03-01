package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.passes.DiffGraph
import org.slf4j.LoggerFactory

/** Creates an AST using [[createAst]].
  * @param filename
  *   the name of the file this file is generated from. This should correspond to the .sol file.
  */
class AstCreator(filename: String, global: Global) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreator])

  val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.add(typeName)
    typeName
  }

  /** Creates the AST for the given Surya [[SourceUnit]].
    * @param sourceUnit
    *   The parsed [[SourceUnit]] representation of a Surya JSON AST.
    */
  def createAst(sourceUnit: SourceUnit): Iterator[DiffGraph] = {
    val astRoot = astForCompilationUnit(sourceUnit)
    storeInDiffGraph(astRoot)
    Iterator(diffGraph.build())
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(ast: Ast): scala.Unit = {
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
  }

  def astForCompilationUnit(value: Any): Ast = {

  }


}
