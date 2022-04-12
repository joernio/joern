package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language._
import overflowdb.Node
import overflowdb.traversal.iterableToTraversal

import scala.jdk.CollectionConverters._

class PagGenerator {

  val edgeType: String = EdgeTypes.POINTS_TO

  def generate(astRoot: AstNode): Graph = {
    def shouldBeDisplayed(v: AstNode): Boolean = v match {
      case _: MethodParameterOut => false
      case _: Modifier           => false
      case _: MethodReturn       => false
      case _: Return             => false
      case _: MethodParameterIn  => false
      case _: Local              => false
      case _                     => true
    }
    val vertices = astRoot.ast.filter(shouldBeDisplayed).l
    val astEdges = vertices.flatMap(v =>
      v.astChildren.filter(shouldBeDisplayed).map { child =>
        Edge(v, child, label = EdgeTypes.AST)
      }
    )
    val pagEdges = vertices.flatMap(v =>
      v.out(edgeType).asScala.collectAll[AstNode].map { dstV =>
        Edge(v, dstV, edgeType = edgeType)
      }
    )
    Graph(vertices, astEdges ++ pagEdges)
  }

}
