package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class AnnotationNodeMethods(val node: Annotation) extends AnyVal with NodeExtension {
  def methodOption: Option[Method] = {
    node.inAst.collectAll[Method].headOption
  }
}
