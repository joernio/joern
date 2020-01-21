/*
 * Adapted from the `cfgToDot` script to produce a single dot file
 * per function found in the provided CPG.
 */

import ammonite.ops._
import org.apache.tinkerpop.gremlin.structure.{ Direction, Vertex }

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.Expression
import java.nio.file.Paths
import java.net.URI

import scala.annotation.tailrec

def getTopLevelExpressions(expression: Vertex): List[Expression] = {
  expression
    .out(EdgeTypes.AST)
    .hasLabel(NodeTypes.BLOCK, NodeTypes.CONTROL_STRUCTURE, NodeTypes.RETURN, NodeTypes.CALL)
    .cast[nodes.Expression]
    .l
}

def dotFromMethod(method: Method): List[String] = {
  @tailrec
  def go(subExprs: List[Expression],
         parent: Option[Expression] = None,
         dots: List[String] = List.empty): List[String] = {

    val parentId = parent.map(_.id.toString).getOrElse(method.id)

    subExprs match {
      case Nil =>
        dots
      case expr :: tail => expr match {
        case ex: Block =>
          val currDotRepr = s""" "$parentId" -> "${ex.id}" [label=BLOCK];"""
          go(getTopLevelExpressions(ex) ::: tail, Some(ex), dots :+ currDotRepr)
        case ex: ControlStructure =>
          val currDotRepr = s""" "$parentId" -> "${ex.id}" [label="${ex.code}"];"""
          go(getTopLevelExpressions(ex) ::: tail, Some(ex), dots :+ currDotRepr)
        case ex: Return =>
          val currDotRepr =  s""" "$parentId" -> "${ex.id}" [label="${ex.code}"];"""
          go(tail, parent, dots :+ currDotRepr)
        case ex: Call =>
          val currDotRepr = s""" "$parentId" -> "${ex.id}" [label="${ex.code}"];"""
          go(tail, parent, dots :+ currDotRepr)
        case _  =>
          // Ignore all other node types.
          go(tail, parent, dots)
      }
    }
  }

  val methodExpressions = method
    .out(EdgeTypes.AST)
    .hasLabel(NodeTypes.BLOCK)
    .out(EdgeTypes.AST)
    .not(_.hasLabel(NodeTypes.LOCAL, NodeTypes.TYPE_DECL))
    .cast[nodes.Expression]
    .l

  go(methodExpressions)
}

def stringifyDotFromMethod(method: Method): String = {
  val sb = new StringBuilder
  sb.append(s"digraph ${method.name} {\n")
  sb.append(dotFromMethod(method).mkString("\n"))
  sb.append("\n}\n")
  sb.toString
}

@main def main(): List[String] = {
  cpg.method.internal.l.map { method =>
    stringifyDotFromMethod(method)
  }
}
