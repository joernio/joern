package io.shiftleft.semanticcpg.dotgenerator

import flatgraph.Accessors
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess
import org.apache.commons.text.StringEscapeUtils

import java.util.Optional
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.language.postfixOps

object DotSerializer {

  private val charLimit = 50

  case class Graph(
    vertices: List[StoredNode],
    edges: List[Edge],
    subgraph: Map[String, Seq[StoredNode]] = HashMap.empty[String, Seq[StoredNode]]
  ) {

    def ++(other: Graph): Graph = {
      Graph((this.vertices ++ other.vertices).distinct, (this.edges ++ other.edges).distinct)
    }

  }
  case class Edge(
    src: StoredNode,
    dst: StoredNode,
    srcVisible: Boolean = true,
    label: String = "",
    edgeType: String = ""
  )

  def dotGraph(root: Option[AstNode] = None, graph: Graph, withEdgeTypes: Boolean = false): String = {
    val sb = root match {
      case Some(r) => namedGraphBegin(r)
      case None    => defaultGraphBegin()
    }
    val nodeStrings = graph.vertices.map(nodeToDot)
    val edgeStrings = graph.edges.map(e => edgeToDot(e, withEdgeTypes))
    val subgraphStrings = graph.subgraph.zipWithIndex.map { case ((subgraph, nodes), idx) =>
      nodesToSubGraphs(subgraph, nodes, idx)
    }
    sb.append((nodeStrings ++ edgeStrings ++ subgraphStrings).mkString("\n"))
    graphEnd(sb)
  }

  private def namedGraphBegin(root: AstNode): mutable.StringBuilder = {
    val sb = new mutable.StringBuilder
    val name = StringEscapeUtils.escapeHtml4(root match {
      case method: Method => method.name
      case _              => ""
    })
    sb.append(s"""digraph "$name" {  \n""")
  }

  private def defaultGraphBegin(): mutable.StringBuilder = {
    val sb   = new mutable.StringBuilder
    val name = "CPG"
    sb.append(s"""digraph "$name" {  \n""")
  }

  private def limit(str: String): String = if (str.length > charLimit) {
    s"${str.take(charLimit - 3)}..."
  } else {
    str
  }

  private def stringRepr(vertex: StoredNode): String = {
    // TODO MP after the initial flatgraph migration (where we want to maintain semantics as far as
    // possible) this might become `vertex.property(Properties.LineNumber)` which derives to `Option[Int]`
    val lineNoMaybe = vertex.propertyOption[Int](PropertyNames.LINE_NUMBER)

    StringEscapeUtils.escapeHtml4(vertex match {
      case call: Call                            => (call.name, limit(call.code)).toString
      case contrl: ControlStructure              => (contrl.label, contrl.controlStructureType, contrl.code).toString
      case expr: Expression                      => (expr.label, limit(expr.code), limit(toCfgNode(expr).code)).toString
      case method: Method                        => (method.label, method.name).toString
      case ret: MethodReturn                     => (ret.label, ret.typeFullName).toString
      case param: MethodParameterIn              => ("PARAM", param.code).toString
      case local: Local                          => (local.label, s"${local.code}: ${local.typeFullName}").toString
      case target: JumpTarget                    => (target.label, target.name).toString
      case modifier: Modifier                    => (modifier.label, modifier.modifierType).toString()
      case annoAssign: AnnotationParameterAssign => (annoAssign.label, annoAssign.code).toString()
      case annoParam: AnnotationParameter        => (annoParam.label, annoParam.code).toString()
      case typ: Type                             => (typ.label, typ.name).toString()
      case typeDecl: TypeDecl                    => (typeDecl.label, typeDecl.name).toString()
      case member: Member                        => (member.label, member.name).toString()
      case _                                     => ""
    }) + lineNoMaybe.map(lineNo => s"<SUB>$lineNo</SUB>").getOrElse("")
  }

  private def toCfgNode(node: StoredNode): CfgNode = {
    node match {
      case node: Identifier         => node.parentExpression.get
      case node: MethodRef          => node.parentExpression.get
      case node: Literal            => node.parentExpression.get
      case node: MethodParameterIn  => node.method
      case node: MethodParameterOut => node.method.methodReturn
      case node: Call if MemberAccess.isGenericMemberAccessName(node.name) =>
        node.parentExpression.get
      case node: CallRepr     => node
      case node: MethodReturn => node
      case node: Expression   => node
    }
  }

  private def nodeToDot(node: StoredNode): String = {
    s""""${node.id}" [label = <${stringRepr(node)}> ]""".stripMargin
  }

  private def edgeToDot(edge: Edge, withEdgeTypes: Boolean): String = {
    val edgeLabel = if (withEdgeTypes) {
      edge.edgeType + ": " + StringEscapeUtils.escapeHtml4(edge.label)
    } else {
      StringEscapeUtils.escapeHtml4(edge.label)
    }
    val labelStr = Some(s""" [ label = "$edgeLabel"] """).filter(_ => edgeLabel != "").getOrElse("")
    s"""  "${edge.src.id}" -> "${edge.dst.id}" """ + labelStr
  }

  def nodesToSubGraphs(subgraph: String, children: Seq[StoredNode], idx: Int): String = {
    val escapedName = StringEscapeUtils.escapeHtml4(subgraph)
    val childString = children.map { c => s"    \"${c.id()}\";" }.mkString("\n")
    s"""  subgraph cluster_$idx {
       |$childString
       |    label = "$escapedName";
       |  }
       |""".stripMargin
  }

  private def graphEnd(sb: mutable.StringBuilder): String = {
    sb.append("\n}\n")
    sb.toString
  }

}
