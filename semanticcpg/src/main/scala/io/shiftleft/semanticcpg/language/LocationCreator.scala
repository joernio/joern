package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec

/* TODO MP: this should be part of the normal steps, rather than matching on the type at runtime
 * all (and only) steps extending DataFlowObject should/must have `newSink`, `newSource` and `newLocation` */
object LocationCreator {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def apply(node: StoredNode)(implicit finder: NodeExtensionFinder): NewLocation = {
    try {
      location(node)
    } catch {
      case exc @ (_: NoSuchElementException | _: ClassCastException) =>
        logger.error(s"Cannot determine location for ${node.label} due to broken CPG", exc)
        emptyLocation(node.label, Some(node))
    }
  }

  private def location(node: StoredNode)(implicit finder: NodeExtensionFinder): NewLocation = {
    finder(node) match {
      case Some(n: HasLocation) => n.location
      case _                    => LocationCreator.emptyLocation("", None)
    }
  }

  def defaultCreateLocation(node: StoredNode, method: Method = null): NewLocation = {
    val location = NewLocation()
      .node(node)
      .nodeLabel(node.label)
      .lineNumber(node.property(Properties.LineNumber))
      .columnNumber(node.property(Properties.ColumnNumber))

    node match {
      case _: Call | _: Literal | _: MethodRef =>
        location.symbol(node.property(Properties.Code))
      case _: Identifier | _: Local | _: MethodParameterIn | _: MethodParameterOut | _: Method =>
        location.symbol(node.property(Properties.Name))
      case _: MethodReturn =>
        location.symbol("$ret")
      case _               =>
    }

    var m = method
    if (m == null) try {
      node match {
        case cfg: CfgNode =>
          m = cfg.method
        case loc: Local =>
          m = loc.method.head
        case _ =>
      }
    } catch {
      case _: Throwable =>
    }
    if (m != null) {
      val typeOption    = methodToTypeDecl(m)
      val typeName      = typeOption.map(_.fullName).getOrElse("")
      val typeShortName = typeOption.map(_.name).getOrElse("")

      val namespaceOption = for {
        tpe            <- typeOption
        namespaceBlock <- tpe.namespaceBlock
        namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
      } yield namespace.name
      val namespaceName = namespaceOption.getOrElse("")

      location
        .methodFullName(m.fullName)
        .methodShortName(m.name)
        .packageName(namespaceName)
        .className(typeName)
        .classShortName(typeShortName)
        .filename(if (m.filename.isEmpty) "N/A" else m.filename)
    }

    location
  }

  def apply(
    node: StoredNode,
    symbol: String,
    label: String,
    lineNumber: Option[Int],
    method: Method,
    columnNumber: Option[Int] = None
  ): NewLocation = {

    if (method == null) {
      NewLocation().node(node)
    } else {
      val typeOption    = methodToTypeDecl(method)
      val typeName      = typeOption.map(_.fullName).getOrElse("")
      val typeShortName = typeOption.map(_.name).getOrElse("")

      val namespaceOption = for {
        tpe            <- typeOption
        namespaceBlock <- tpe.namespaceBlock
        namespace      <- namespaceBlock._namespaceViaRefOut.nextOption()
      } yield namespace.name
      val namespaceName = namespaceOption.getOrElse("")

      NewLocation()
        .symbol(symbol)
        .methodFullName(method.fullName)
        .methodShortName(method.name)
        .packageName(namespaceName)
        .lineNumber(lineNumber)
        .className(typeName)
        .classShortName(typeShortName)
        .nodeLabel(label)
        .filename(if (method.filename.isEmpty) "N/A" else method.filename)
        .node(node)
    }
  }

  private def methodToTypeDecl(method: Method): Option[TypeDecl] =
    findVertex(method, _.isInstanceOf[TypeDecl]).map(_.asInstanceOf[TypeDecl])

  @tailrec
  private def findVertex(node: StoredNode, instanceCheck: StoredNode => Boolean): Option[StoredNode] =
    node._astIn.iterator.nextOption() match {
      case Some(head) if instanceCheck(head) => Some(head)
      case Some(head)                        => findVertex(head, instanceCheck)
      case None                              => None
    }

  def emptyLocation(label: String, node: Option[StoredNode]): NewLocation =
    NewLocation().nodeLabel(label).node(node)
}
