package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal._

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

  def apply(node: StoredNode, symbol: String, label: String, lineNumber: Option[Int], method: Method): NewLocation = {

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
    node._astIn.nextOption() match {
      case Some(head) if instanceCheck(head) => Some(head)
      case Some(head)                        => findVertex(head, instanceCheck)
      case None                              => None
    }

  def emptyLocation(label: String, node: Option[StoredNode]): NewLocation =
    NewLocation().nodeLabel(label).node(node)
}
