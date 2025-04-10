package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.annotation.tailrec

class Loc(val node: StoredNode) {
  def symbol: String = {
    node match {
      case call: Call                   => call.code
      case method: Method               => method.name
      case inParam: MethodParameterIn   => inParam.name
      case ident: Identifier            => ident.name
      case lit: Literal                 => lit.code
      case local: Local                 => local.name
      case outParam: MethodParameterOut => outParam.name
      case methodRef: MethodRef         => methodRef.code
      case methodReturn: MethodReturn   => "$ret"
      case _                            => defaultString
    }
  }

  def nodeLabel: String = node.label

  def lineNumber: Option[Int] = node match {
    case astNode: AstNode => astNode.lineNumber
    case cfgNode: CfgNode => cfgNode.lineNumber
  }

  def methodFullName: String = method.fullName

  def methodShortName: String = method.name

  def packageName: String = namespaceOption.getOrElse(defaultString)

  def className: String = typeOption.map(_.fullName).getOrElse(defaultString)

  def classShortName: String = typeOption.map(_.name).getOrElse(defaultString)

  def filename: String = if (method.filename.isEmpty) "N/A" else method.filename

  private val defaultString = "<empty>";

  private lazy val typeOption = findAncestor[TypeDecl](method)

  private lazy val namespaceOption = for {
    tpe            <- typeOption
    namespaceBlock <- tpe.namespaceBlock
    namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
  } yield namespace.name

  private lazy val method: Method = node match {
    case cfgNode: CfgNode => cfgNode.method
    case local: Local     => local.method.head
  }

  @tailrec
  private def findAncestor[TargetNodeType <: StoredNode](node: StoredNode): Option[TargetNodeType] = {
    node._astIn.iterator.nextOption() match {
      case Some(head) if head.isInstanceOf[TargetNodeType] => Some(head.asInstanceOf[TargetNodeType])
      case Some(head)                                      => findAncestor[TargetNodeType](head)
      case None                                            => None
    }
  }
}
