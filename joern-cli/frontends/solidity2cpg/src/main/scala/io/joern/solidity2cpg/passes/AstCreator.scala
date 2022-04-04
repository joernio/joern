package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

/** Creates an AST using [[createAst]].
  * @param filename
  *   the name of the file this file is generated from. This should correspond to the .sol file.
  * @param sourceUnit
  *   the parsed [[SourceUnit]] representation of a Surya JSON AST.
  * @param global
  *   shared project-wide information.
  */
class AstCreator(filename: String, sourceUnit: SourceUnit, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Creates the AST for the given Surya [[SourceUnit]].
    * @return
    *   the changes associated to building the AST.
    */
  override def createAst(): DiffGraphBuilder = {
    val astRoot = astForCompilationUnit(sourceUnit)
    storeInDiffGraph(astRoot)
    diffGraph
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
    Ast()
  }

}
