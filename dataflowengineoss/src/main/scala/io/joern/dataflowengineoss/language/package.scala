package io.joern.dataflowengineoss

import io.shiftleft.codepropertygraph.generated.nodes._
import io.joern.dataflowengineoss.language.dotextension.DdgNodeDot
import io.joern.dataflowengineoss.language.nodemethods.{ExpressionMethods, ExtendedCfgNodeMethods}
import overflowdb.traversal._

package object language {

  implicit def cfgNodeToMethodsQp[NodeType <: CfgNode](node: NodeType): ExtendedCfgNodeMethods[NodeType] =
    new ExtendedCfgNodeMethods(node)

  implicit def expressionMethods[NodeType <: Expression](node: NodeType): ExpressionMethods[NodeType] =
    new ExpressionMethods(node)

  implicit def toExtendedCfgNode[NodeType <: CfgNode](traversal: IterableOnce[NodeType]): ExtendedCfgNode =
    new ExtendedCfgNode(traversal)

  implicit def toDdgNodeDot(traversal: IterableOnce[Method]): DdgNodeDot =
    new DdgNodeDot(traversal)

  implicit def toDdgNodeDotSingle(method: Method): DdgNodeDot =
    new DdgNodeDot(Traversal.fromSingle(method))

}
