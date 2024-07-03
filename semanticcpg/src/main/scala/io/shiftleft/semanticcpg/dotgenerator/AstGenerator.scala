package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, MethodParameterOut}
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language.*

class AstGenerator {

  private val edgeType = EdgeTypes.AST

  def generate(astRoot: AstNode): Graph = {
    def shouldBeDisplayed(v: AstNode): Boolean = !v.isInstanceOf[MethodParameterOut]
    val vertices                               = astRoot.ast.filter(shouldBeDisplayed).l
    val edges = vertices.flatMap(v =>
      v.astChildren.filter(shouldBeDisplayed).map { child =>
        Edge(v, child, edgeType = edgeType)
      }
    )
    Graph(vertices, edges)
  }

}
