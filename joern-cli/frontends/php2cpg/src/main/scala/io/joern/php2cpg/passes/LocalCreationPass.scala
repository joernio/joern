package io.joern.php2cpg.passes

import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  Identifier,
  Method,
  NamespaceBlock,
  NewLocal,
  NewNode,
  TypeDecl
}
import io.shiftleft.semanticcpg.language.*
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.Domain
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.x2cpg.AstNodeBuilder
import io.shiftleft.codepropertygraph.generated.PropertyNames

object LocalCreationPass {
  def allLocalCreationPasses(cpg: Cpg): Iterator[LocalCreationPass[? <: AstNode]] =
    Iterator(new NamespaceLocalPass(cpg), new MethodLocalPass(cpg))
}

abstract class LocalCreationPass[ScopeType <: AstNode](cpg: Cpg)
    extends ForkJoinParallelCpgPass[ScopeType](cpg)
    with AstNodeBuilder[AstNode, LocalCreationPass[ScopeType]] {
  override protected def line(node: AstNode)                   = node.lineNumber
  override protected def column(node: AstNode)                 = node.columnNumber
  override protected def lineEnd(node: AstNode): Option[Int]   = None
  override protected def columnEnd(node: AstNode): Option[Int] = None
  override protected def code(node: AstNode): String           = node.code

  protected def getIdentifiersInScope(node: AstNode): List[Identifier] = {
    node match {
      case identifier: Identifier                              => identifier :: Nil
      case _: TypeDecl | _: Method | _: NamespaceBlock         => Nil
      case _ if node.astChildren.isEmpty                       => Nil
      case call: Call if call.name == PhpOperators.declareFunc =>
        // TODO Handle declares properly
        // but for now don't change behaviour.
        Nil
      case _ => node.astChildren.flatMap(getIdentifiersInScope).toList
    }
  }

  protected def localsForIdentifiers(
    identifierMap: Map[String, List[Identifier]]
  ): List[(NewLocal, List[Identifier])] = {
    identifierMap
      .map { case identifierName -> identifiers =>
        val code = s"$$$identifierName"
        val local =
          localNode(identifiers.head, identifierName, code, AstCreator.TypeConstants.Any, closureBindingId = None)
        (local -> identifiers)
      }
      .toList
      .sortBy { case (local, _) => local.name }
  }

  protected def addRefEdges(diffGraph: DiffGraphBuilder, localPairs: List[(NewLocal, List[Identifier])]): Unit = {
    localPairs.foreach { case (local, identifiers) =>
      identifiers.foreach { identifier =>
        diffGraph.addEdge(identifier, local, EdgeTypes.REF)
      }
    }
  }

  protected def prependLocalsToBody(diffGraph: DiffGraphBuilder, bodyNode: AstNode, locals: List[NewLocal]): Unit = {
    val originalChildren = bodyNode.astChildren.l

    bodyNode.outE(EdgeTypes.AST).foreach(diffGraph.removeEdge)

    locals.zipWithIndex.foreach { case (local, idx) =>
      local.order(idx + 1)
    }

    val localCount = locals.size

    originalChildren.foreach { node =>
      diffGraph.setNodeProperty(node, PropertyNames.ORDER, node.order + localCount)
    }

    (locals ++ originalChildren).foreach { node =>
      diffGraph.addEdge(bodyNode, node, EdgeTypes.AST)
    }
  }

  protected def addLocalsToAst(
    diffGraph: DiffGraphBuilder,
    bodyNode: AstNode,
    excludeIdentifierFn: Identifier => Boolean
  ): Unit = {
    val identifierMap =
      getIdentifiersInScope(bodyNode)
        .filter(_._refOut.isEmpty)
        .filterNot(excludeIdentifierFn)
        .groupBy(_.name)

    val localPairs = localsForIdentifiers(identifierMap)

    if (localPairs.nonEmpty) {
      val locals = localPairs.map { case (local, _) => local }

      addRefEdges(diffGraph, localPairs)
      prependLocalsToBody(diffGraph, bodyNode, locals)
    }
  }
}

class NamespaceLocalPass(cpg: Cpg) extends LocalCreationPass[NamespaceBlock](cpg) {
  override def generateParts(): Array[NamespaceBlock] = cpg.namespaceBlock.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, namespace: NamespaceBlock): Unit = {
    addLocalsToAst(diffGraph, namespace, excludeIdentifierFn = _ => false)
  }
}

class MethodLocalPass(cpg: Cpg) extends LocalCreationPass[Method](cpg) {
  override def generateParts(): Array[Method] = cpg.method.internal.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val parameters = method.parameter.name.toSet
    addLocalsToAst(diffGraph, method.body, excludeIdentifierFn = identifier => parameters.contains(identifier.name))
  }
}
