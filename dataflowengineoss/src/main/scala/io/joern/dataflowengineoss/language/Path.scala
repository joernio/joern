package io.joern.dataflowengineoss.language

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, Member, MethodParameterIn}
import io.shiftleft.semanticcpg
import io.shiftleft.semanticcpg.language.*
import flatgraph.help.Table
import flatgraph.help.Table.AvailableWidthProvider

case class Path(elements: List[AstNode]) {
  def resultPairs(): List[(String, Option[Int])] = {
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

  implicit def show(implicit
    availableWidthProvider: AvailableWidthProvider = semanticcpg.defaultAvailableWidthProvider
  ): Show[Path] = { path =>
    val table = Table(
      columnNames = Array("nodeType", "tracked", "line", "method", "file"),
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
            Array(nodeType, statement, lineNumber, methodName, fileName)
        }
      }
    )
    // add a line break for nicer repl rendering
    "\n" + table.render
  }

}
