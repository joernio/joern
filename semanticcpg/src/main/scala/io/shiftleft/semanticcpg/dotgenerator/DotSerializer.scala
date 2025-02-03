package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess
import org.apache.commons.lang3.StringUtils
import org.apache.commons.text.StringEscapeUtils

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.language.postfixOps

object DotSerializer {

  private val CharLimit = 50

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

    sb.append(s"""node [shape="rect"];  \n""")
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

  private def limit(str: String): String = StringUtils.abbreviate(str, CharLimit)

  private def stringRepr(vertex: StoredNode): String = {
    val lineOpt = vertex.property(Properties.LineNumber).map(_.toString)
    val attrList = (vertex match {
      case call: Call                            => List(call.name, limit(call.code))
      case ctrl: ControlStructure                => List(ctrl.label, ctrl.controlStructureType, ctrl.code)
      case expr: Expression                      => List(expr.label, limit(expr.code), limit(toCfgNode(expr).code))
      case method: Method                        => List(method.label, method.name)
      case ret: MethodReturn                     => List(ret.label, ret.typeFullName)
      case param: MethodParameterIn              => List("PARAM", param.code)
      case local: Local                          => List(local.label, s"${local.code}: ${local.typeFullName}")
      case target: JumpTarget                    => List(target.label, target.name)
      case modifier: Modifier                    => List(modifier.label, modifier.modifierType)
      case annoAssign: AnnotationParameterAssign => List(annoAssign.label, annoAssign.code)
      case annoParam: AnnotationParameter        => List(annoParam.label, annoParam.code)
      case typ: Type                             => List(typ.label, typ.name)
      case typeDecl: TypeDecl                    => List(typeDecl.label, typeDecl.name)
      case member: Member                        => List(member.label, member.name)
      case _                                     => List.empty
    }).map(l => StringEscapeUtils.escapeHtml4(StringUtils.normalizeSpace(l)))

    (lineOpt match {
      case Some(line) => s"${attrList.head}, $line" :: attrList.tail
      case None       => attrList
    }).distinct.mkString("<BR/>")
  }

  private def toCfgNode(node: StoredNode): CfgNode = {
    node match {
      case node: Identifier                                                => node.parentExpression.get
      case node: MethodRef                                                 => node.parentExpression.get
      case node: Literal                                                   => node.parentExpression.get
      case node: Call if MemberAccess.isGenericMemberAccessName(node.name) => node.parentExpression.get
      case node: MethodParameterOut                                        => node.method.methodReturn
      case node: MethodParameterIn                                         => node.method
      case node: CallRepr                                                  => node
      case node: MethodReturn                                              => node
      case node: Expression                                                => node
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

  private def nodesToSubGraphs(subgraph: String, children: Seq[StoredNode], idx: Int): String = {
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
