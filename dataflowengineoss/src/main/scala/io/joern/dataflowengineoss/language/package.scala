package io.joern.dataflowengineoss

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.dotextension.DdgNodeDot
import io.joern.dataflowengineoss.language.nodemethods.{ExpressionMethods, ExtendedCfgNodeMethods}
import overflowdb.traversal.help.Doc

import scala.language.implicitConversions

package object language {

  implicit def cfgNodeToMethodsQp[NodeType <: CfgNode](node: NodeType): ExtendedCfgNodeMethods[NodeType] =
    new ExtendedCfgNodeMethods(node)

  implicit def expressionMethods[NodeType <: Expression](node: NodeType): ExpressionMethods[NodeType] =
    new ExpressionMethods(node)

  implicit def toExtendedCfgNode[NodeType <: CfgNode](traversal: IterableOnce[NodeType]): ExtendedCfgNode =
    new ExtendedCfgNode(traversal.iterator)

  implicit def toDdgNodeDot(traversal: IterableOnce[Method]): DdgNodeDot =
    new DdgNodeDot(traversal.iterator)

  implicit def toDdgNodeDotSingle(method: Method): DdgNodeDot =
    new DdgNodeDot(Iterator.single(method))

  implicit def toExtendedPathsTrav[NodeType <: Path](traversal: IterableOnce[NodeType]): PassesExt =
    new PassesExt(traversal.iterator)

  class PassesExt(traversal: Iterator[Path]) {

    @Doc(info = "Filters in paths that pass though the given paths")
    def passes(trav: Iterator[AstNode] => Iterator[?]): Iterator[Path] = {
      traversal.filter(_.elements.exists(_.start.where(trav).nonEmpty))
    }

    @Doc(info = "Filters out paths that pass though the given paths")
    def passesNot(trav: Iterator[AstNode] => Iterator[?]): Iterator[Path] = {
      traversal.filter(_.elements.forall(_.start.where(trav).isEmpty))
    }

  }

}
