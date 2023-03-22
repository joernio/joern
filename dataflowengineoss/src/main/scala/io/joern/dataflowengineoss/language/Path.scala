package io.joern.dataflowengineoss.language

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, Member, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.apache.commons.lang.StringUtils
import overflowdb.traversal.help.Table

case class Path(elements: List[AstNode]) {
  def resultPairs(): List[(String, Option[Integer])] = {
    val pairs = elements.map {
      case point: MethodParameterIn =>
        val method      = point.method
        val method_name = method.name
        val code        = s"$method_name(${method.parameter.l.sortBy(_.order).map(_.code).mkString(", ")})"
        (code, point.lineNumber)
      case point => (point.statement.repr, point.lineNumber)
    }
    pairs.headOption.map(x => x :: pairs.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }
}

object Path {

  val DefaultMaxTrackedWidth = 30
  // TODO replace with dynamic rendering based on the terminal's width, e.g. in scala-repl-pp
  lazy val maxTrackedWidth = sys.env.get("JOERN_DATAFLOW_TRACKED_WIDTH").map(_.toInt).getOrElse(DefaultMaxTrackedWidth)

  implicit val show: Show[Path] = { path =>
    Table(
      columnNames = Array("nodeType", "tracked", "lineNumber", "method", "file"),
      rows = path.elements.map { astNode =>
        val nodeType   = astNode.getClass.getSimpleName
        val lineNumber = astNode.lineNumber.getOrElse("N/A").toString
        val fileName   = astNode.file.name.headOption.getOrElse("N/A")

        astNode match {
          case member: Member =>
            val tracked    = member.name
            val methodName = "<not-in-method>"
            Array(nodeType, tracked, lineNumber, methodName, fileName)
          case cfgNode: CfgNode =>
            val method     = cfgNode.method
            val methodName = method.name
            val statement = cfgNode match {
              case _: MethodParameterIn =>
                val paramsPretty = method.parameter.toList.sortBy(_.index).map(_.code).mkString(", ")
                s"$methodName($paramsPretty)"
              case _ => cfgNode.statement.repr
            }
            val tracked = StringUtils.normalizeSpace(StringUtils.abbreviate(statement, maxTrackedWidth))
            Array(nodeType, tracked, lineNumber, methodName, fileName)
        }
      }
    ).render
  }

}
