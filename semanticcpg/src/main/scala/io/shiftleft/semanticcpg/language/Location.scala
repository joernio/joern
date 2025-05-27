package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Properties
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

// Previously, the non-stored Location node generated
// from the CPG schema was named NewLocation. The
// addition of lazily computed location info in this
// file brought the deletion of these CPG nodes. This
// is not directly used in joern, but intended to not
// break usage in users of the API. Users directly
// importing NewLocation from the CPG package rather
// will still have a break.
@deprecated("NewLocation is deprecated, prefer LocationInfo")
type NewLocation = LocationInfo

trait HasLocation extends Any {
  def location: LocationInfo
}

// This design: 1) lets implementors put all logic for
// determining how to fill out `LocationInfo` for each
// type of node in one central place, and 2) allow consumers
// of joern, which may also provide different node types
// or location information, to provide alternate or
// extended implementations of LocCreator.
trait LocationCreator {
  implicit def apply(node: AbstractNode): LocationInfo
}

implicit object Location extends LocationCreator {
  implicit def apply(node: AbstractNode): LocationInfo = {
    node match {
      case storedNode: StoredNode => LazyLocation(storedNode)
      case _                      => EmptyLocation
    }
  }
}

object EmptyLocation extends LocationInfo {
  override def node: Option[AbstractNode] = None
  override def symbol: String             = "<empty>"
  override def nodeLabel: String          = "<empty>"
  override def lineNumber: Option[Int]    = None
  override def methodFullName: String     = "<empty>"
  override def methodShortName: String    = "<empty>"
  override def packageName: String        = "<empty>"
  override def className: String          = "<empty>"
  override def classShortName: String     = "<empty>"
  override def filename: String           = "<empty>"
}

class LazyLocation(storedNode: StoredNode) extends LocationInfo {
  def node: Option[AbstractNode] = Some(storedNode)

  def symbol: String = {
    storedNode match {
      case _: Call | _: Literal | _: MethodRef =>
        storedNode.property(Properties.Code)
      case _: Identifier | _: Local | _: MethodParameterIn | _: MethodParameterOut | _: Method =>
        storedNode.property(Properties.Name)
      case _: MethodReturn => "$ret"
      case _               => defaultString
    }
  }

  def nodeLabel: String = storedNode.label

  def lineNumber: Option[Int] = storedNode match {
    case astNode: AstNode => astNode.lineNumber
    case _                => None
  }

  def methodFullName: String = method.fullName

  def methodShortName: String = method.name

  def packageName: String = namespaceOption.getOrElse(defaultString)

  def className: String = typeOption.map(_.fullName).getOrElse(defaultString)

  def classShortName: String = typeOption.map(_.name).getOrElse(defaultString)

  def filename: String = if (method.filename.isEmpty) "N/A" else method.filename

  final protected val defaultString = "<empty>";

  private lazy val typeOption = findParentTypeDecl(method)

  private lazy val namespaceOption = for {
    tpe            <- typeOption
    namespaceBlock <- tpe.namespaceBlock
    namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
  } yield namespace.name

  private lazy val method: Method = storedNode match {
    case cfgNode: CfgNode => cfgNode.method
    case local: Local     => local.method.head
  }

  @tailrec
  private def findParentTypeDecl(node: StoredNode): Option[TypeDecl] = {
    node._astIn.iterator.nextOption() match {
      case Some(head) if head.isInstanceOf[TypeDecl] => Some(head.asInstanceOf[TypeDecl])
      case Some(head)                                => findParentTypeDecl(head)
      case None                                      => None
    }
  }
}
