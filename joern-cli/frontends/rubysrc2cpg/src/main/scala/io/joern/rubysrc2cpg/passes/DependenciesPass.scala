package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.parser.jruby.JRubyBasedParser
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.jruby.ast.{ArrayNode, FCallNode, Node, StrNode}
import org.jruby.ast.visitor._
import org.slf4j.LoggerFactory

import scala.util.Try

class DependenciesPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    cpg.configFile.filter(_.name.endsWith("Gemfile")).foreach { gemFile =>
      parseGemCalls(gemFile.name, gemFile.content)
        .map {
          case gemCall if gemCall.gemVersion.isDefined =>
            NewDependency().name(gemCall.gemName).version(gemCall.gemVersion.get)
          case gemCall => NewDependency().name(gemCall.gemName)
        }
        .foreach { diffGraph.addNode }
    }
  }

  private def parseGemCalls(filename: String, code: String): Set[GemCall] =
    Try(JRubyBasedParser.parseString(code, "Gemfile")).toOption match {
      case Some(rootNode) =>
        val visitor = new GemCallVisitor
        rootNode.accept(visitor)
        visitor.gemCalls
      case None =>
        logger.debug(s"Failed to parse '$filename'")
        Set()
    }

  private case class GemCall(gemName: String, gemVersion: Option[String])

  private class GemCallVisitor extends AbstractNodeVisitor[Unit] {
    var gemCalls: Set[GemCall] = Set()

    override def defaultVisit(node: Node): Unit = visitChildren(node)

    override def visitFCallNode(node: FCallNode): Unit = parseGemCall(node) match {
      case Some(gemCall) => gemCalls += gemCall
      case None          => defaultVisit(node)
    }

    private def parseGemCall(node: FCallNode): Option[GemCall] = (node.getName.idString(), node.getArgsNode) match {
      case ("gem", args: ArrayNode) if args.children().forall(_.isInstanceOf[StrNode]) =>
        val name    = args.children().lift(0).map(_.asInstanceOf[StrNode].getValue.toByteString)
        val version = args.children().lift(1).map(_.asInstanceOf[StrNode].getValue.toByteString)
        name.map(GemCall(_, version))
      case _ => None
    }
  }
}
