package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.annotation.tailrec

trait LocationInfo {
  def node: Option[AbstractNode]
  def symbol: String
  def nodeLabel: String
  def lineNumber: Option[Int]
  def methodFullName: String
  def methodShortName: String
  def packageName: String
  def className: String
  def classShortName: String
  def filename: String
}

trait LocCreator {
  implicit def apply(node: AbstractNode): LocationInfo
}

implicit object Loc extends LocCreator {
  implicit def apply(node: AbstractNode): LocationInfo = {
    node match {
      case storedNode: StoredNode => LazyLoc(storedNode)
      case _ => EmptyLoc
    }
  }
}

object EmptyLoc extends LocationInfo {
  override def node: Option[AbstractNode] = None
  override def symbol: String = "<empty>"
  override def nodeLabel: String = "<empty>"
  override def lineNumber: Option[Int] = None
  override def methodFullName: String = "<empty>"
  override def methodShortName: String = "<empty>"
  override def packageName: String = "<empty>"
  override def className: String = "<empty>"
  override def classShortName: String = "<empty>"
  override def filename: String = "<empty>"
}

class LazyLoc(storedNode: StoredNode) extends LocationInfo {
  def node: Option[AbstractNode] = Some(storedNode)
  
  def symbol: String = {
    storedNode match {
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

  def nodeLabel: String = storedNode.label

  def lineNumber: Option[Int] = storedNode match {
    case astNode: AstNode => astNode.lineNumber
    case cfgNode: CfgNode => cfgNode.lineNumber
  }

  def methodFullName: String = method.fullName

  def methodShortName: String = method.name

  def packageName: String = namespaceOption.getOrElse(defaultString)

  def className: String = typeOption.map(_.fullName).getOrElse(defaultString)

  def classShortName: String = typeOption.map(_.name).getOrElse(defaultString)

  def filename: String = if (method.filename.isEmpty) "N/A" else method.filename

  protected val defaultString = "<empty>";

  protected lazy val typeOption = findAncestor[TypeDecl](method)

  protected lazy val namespaceOption = for {
    tpe            <- typeOption
    namespaceBlock <- tpe.namespaceBlock
    namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
  } yield namespace.name

  protected lazy val method: Method = storedNode match {
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
