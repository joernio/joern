package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{AnnotationLiteral, Method}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class AnnotationLiteralNodeMethods(val node: AnnotationLiteral) extends AnyVal with NodeExtension {
  def methodOption: Option[Method] = {
    node.inAst.collectAll[Method].headOption
  }
}
