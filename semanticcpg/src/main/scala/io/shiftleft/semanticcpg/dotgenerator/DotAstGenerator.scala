package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

object DotAstGenerator {

  def dotAst[T <: AstNode](traversal: Iterator[T]): Iterator[String] =
    traversal.map(dotAst)

  def dotAst(astRoot: AstNode): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast)
  }

}
