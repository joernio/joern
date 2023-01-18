package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, NamespaceBlock}
import overflowdb.traversal._
import io.shiftleft.semanticcpg.language._

class ImportTraversal(val traversal: Traversal[Import]) extends AnyVal {

  def call: Traversal[Call] = traversal.in(EdgeTypes.IS_CALL_FOR_IMPORT).cast[Call]

  def namespaceBlock: Traversal[NamespaceBlock] = call.method.namespaceBlock

  def code: Traversal[String] = call.code

  def code(regex : String): Traversal[Import] = traversal.where(_.call.code(regex))

}
