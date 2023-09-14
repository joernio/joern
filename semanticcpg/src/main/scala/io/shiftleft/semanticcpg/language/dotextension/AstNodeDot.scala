package io.shiftleft.semanticcpg.language.dotextension

import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import io.shiftleft.semanticcpg.language.*

class AstNodeDot[NodeType <: AstNode](val traversal: Iterator[NodeType]) extends AnyVal {

  def dotAst: Iterator[String] = DotAstGenerator.dotAst(traversal)

  def plotDotAst(implicit viewer: ImageViewer): Unit = {
    Shared.plotAndDisplay(dotAst.l, viewer)
  }

}
