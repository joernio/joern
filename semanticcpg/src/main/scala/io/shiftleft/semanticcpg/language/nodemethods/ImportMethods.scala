package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, NamespaceBlock}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class ImportMethods(val node: Import) extends AnyVal with NodeExtension {

  def call: Call = Traversal.fromSingle(node).call.head

  def namespaceBlock: NamespaceBlock = Traversal.fromSingle(node).namespaceBlock.head

  def lineNumber: Option[Integer] = node.call.lineNumber

  def columnNumber: Option[Integer] = node.call.columnNumber

  def code: String = node.call.code

}
