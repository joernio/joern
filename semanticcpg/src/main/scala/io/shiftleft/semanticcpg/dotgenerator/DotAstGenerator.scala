package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import overflowdb.traversal._

object DotAstGenerator {

  def dotAst[T <: AstNode](traversal: Traversal[T]): Traversal[String] =
    traversal.map(dotAst)

  def dotAst(astRoot: AstNode): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(astRoot, ast)
  }

}
