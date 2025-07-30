package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Properties, PropertyNames}
import io.shiftleft.semanticcpg.language.nodemethods.AstNodeMethods
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.Steps.JsonSerializeAsProduct

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

// This design: 1) lets implementors put all logic for
// determining how to fill out `LocationInfo` for each
// type of node in one central place, and 2) allow consumers
// of joern, which may also provide different node types
// and/or location information, to provide alternate or
// extended implementations of LocationCreator.
trait LocationCreator {
  implicit def apply(node: StoredNode): LocationInfo
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

object LazyLocation extends LocationCreator {
  implicit def apply(node: StoredNode): LocationInfo = {
    new LazyLocation(node)
  }
}

implicit val locationCreator: LocationCreator = LazyLocation

class LazyLocation(storedNode: StoredNode) extends LocationInfo with JsonSerializeAsProduct {
  def node: Option[AbstractNode] = Option(storedNode)

  def symbol: String = {
    storedNode match {
      case _: Identifier | _: Local | _: MethodParameterIn | _: MethodParameterOut | _: Method =>
        storedNode.property(Properties.Name)
      case _: MethodReturn     => "$ret"
      case astNode: AstNode    => AstNodeMethods(astNode).sourceCode
      case cfgFile: ConfigFile => cfgFile.content
      case _                   => defaultString
    }
  }

  def nodeLabel: String = storedNode.label

  def lineNumber: Option[Int] = storedNode.propertyOption[Int](PropertyNames.LineNumber)

  def methodFullName: String = method.map(_.fullName).getOrElse(defaultString)

  def methodShortName: String = method.map(_.name).getOrElse(defaultString)

  def packageName: String = namespaceOption.getOrElse(defaultString)

  def className: String = typeOption.map(_.fullName).getOrElse(defaultString)

  def classShortName: String = typeOption.map(_.name).getOrElse(defaultString)

  def filename: String = method match {
    case Some(method) if method.filename.nonEmpty => method.filename
    case _ =>
      typeOption.map(_.filename).filterNot(_ == defaultString).getOrElse("N/A")
  }

  final protected val defaultString = "<empty>";

  private lazy val typeOption: Option[TypeDecl] = findParentTypeDecl(storedNode)

  private lazy val namespaceOption = for {
    tpe            <- typeOption
    namespaceBlock <- tpe.namespaceBlock
    namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
  } yield namespace.name

  private lazy val method: Option[Method] = storedNode match {
    case cfgNode: CfgNode => Option(cfgNode.method)
    case _                => findParentMethod(storedNode)
  }

  @tailrec
  private def findParentMethod(node: StoredNode): Option[Method] = {
    node._astIn.iterator.nextOption() match {
      case Some(head: Method) => Option(head)
      case Some(head)         => findParentMethod(head)
      case None               => None
    }
  }

  @tailrec
  private def findParentTypeDecl(node: StoredNode): Option[TypeDecl] = {
    node._astIn.iterator.nextOption() match {
      case Some(head) if head.isInstanceOf[TypeDecl] => Some(head.asInstanceOf[TypeDecl])
      case Some(head)                                => findParentTypeDecl(head)
      case None                                      => None
    }
  }

  // The product implementation is required for json serialization. See io.shiftleft.semanticcpg.language.Steps
  private lazy val productElements: Array[(String, Any)] = Array(
    ("packageName", packageName),
    ("symbol", symbol),
    ("filename", filename),
    ("className", className),
    ("methodFullName", methodFullName),
    ("classShortName", classShortName),
    ("methodShortName", methodShortName),
    ("node", node),
    ("_label", "LOCATION"),
    ("nodeLabel", nodeLabel),
    ("lineNumber", lineNumber)
  )

  override def canEqual(that: Any): Boolean = that != null && that.isInstanceOf[LazyLocation]

  override def productArity: Int = productElements.length

  override def productElementName(n: Int): String = {
    productElements(n)._1
  }

  override def productElement(n: Int): Any = {
    productElements(n)._2
  }

}
