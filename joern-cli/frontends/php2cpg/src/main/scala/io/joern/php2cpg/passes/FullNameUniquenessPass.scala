package io.joern.php2cpg.passes

import io.joern.php2cpg.parser.Domain
import io.shiftleft.codepropertygraph.generated.nodes.{Method, NamespaceBlock, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

/** Ensures that method, type decl, and namespace block fullNames are unique across the CPG. In PHP code, the same
  * top-level class or function name may be declared in multiple files (they are never loaded together at runtime, e.g.
  * an admin and a frontend variant of a class). Joern folds every file into a single CPG, so these declarations collide
  * on their fullName.
  *
  * The CPG specification requires unique fullNames for all kind of linking and querying operations.
  *
  * This unique fullName generation cannot be performed during the initial AST traversal because:
  *   - The AST is built on a per-file basis where we don't have knowledge of methods defined in other files.
  *   - Name uniqueness must be established across the entire codebase, not just within a single file.
  *   - Using a global cache during parsing would slow down our parallel processing.
  *
  * Instead, this pass runs after all methods, type decls, and namespaces have been added to the CPG, identifies
  * duplicates, and appends a unique suffix to ensure each one can be uniquely identified. Calls resolved (via the naive
  * namespace-by-filename approach) to a renamed method in the same file are updated to reflect the new fullName.
  */
class FullNameUniquenessPass(cpg: Cpg) extends CpgPass(cpg) {

  private type AffectedNodeType = Method | TypeDecl | NamespaceBlock
  private val logger = LoggerFactory.getLogger(classOf[FullNameUniquenessPass])

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    handleMethods(dstGraph)
    handleTypeDecls(dstGraph)
    handleNamespaceBlocks(dstGraph)
  }

  private def handleMethods(dstGraph: DiffGraphBuilder): Unit = {
    val methodFullNameMap = generateStableFullNameMapping(cpg.method.nameNot(NamespaceTraversal.globalNamespaceName))
    methodFullNameMap.foreach { case (fullName, methods) =>
      logDuplicates(methods, fullName)
      val sortedMethods = sortNodesByLocation(methods)
      lazy val callsAffected =
        cpg.call.methodFullNameExact(fullName).filterNot(_.file.exists(_.name == sortedMethods.head.filename)).l
      sortedMethods.tail.zipWithIndex.foreach { case (method, index) =>
        val suffix      = s"${Domain.DuplicateSuffix}$index"
        val newFullName = s"$fullName$suffix"
        dstGraph.setNodeProperty(method, PropertyNames.FullName, newFullName)
        // Fix up calls resolved to this method within the same file via the naive namespace-by-filename approach.
        val callCandidates = callsAffected.filter(_.file.exists(_.name == method.filename))
        callCandidates.foreach { call =>
          dstGraph.setNodeProperty(call, PropertyNames.MethodFullName, newFullName)
        }
      }
    }
  }

  private def handleTypeDecls(dstGraph: DiffGraphBuilder): Unit = {
    handleDuplicateFullNames(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName), dstGraph)
  }

  private def handleNamespaceBlocks(dstGraph: DiffGraphBuilder): Unit = {
    handleDuplicateFullNames(cpg.namespaceBlock.nameNot(NamespaceTraversal.globalNamespaceName), dstGraph)
  }

  private def handleDuplicateFullNames[T <: AffectedNodeType](nodes: Iterator[T], dstGraph: DiffGraphBuilder): Unit = {
    val fullNameMap = generateStableFullNameMapping(nodes)
    fullNameMap.foreach { case (fullName, nodesList) =>
      logDuplicates(nodesList, fullName)
      val sortedNodes = sortNodesByLocation(nodesList)
      sortedNodes.tail.zipWithIndex.foreach { case (node, index) =>
        val suffix      = s"${Domain.DuplicateSuffix}$index"
        val newFullName = s"$fullName$suffix"
        dstGraph.setNodeProperty(node, PropertyNames.FullName, newFullName)
      }
    }
  }

  private def generateStableFullNameMapping[T <: AffectedNodeType](nodes: Iterator[T]): Seq[(String, List[T])] = {
    nodes
      .groupBy(_.fullName)
      .filter(_._2.size > 1)
      .sortBy(_._1)
  }

  private def sortNodesByLocation[T <: AffectedNodeType](nodes: List[T]): List[T] = {
    nodes.sortBy { node =>
      val fileName = node.filename
      val lineNr   = node.lineNumber.getOrElse(-1)
      val columnNr = node.columnNumber.getOrElse(-1)
      (fileName, lineNr, columnNr)
    }
  }

  private def logDuplicates[T <: AffectedNodeType](nodes: List[T], fullName: String): Unit = {
    if (logger.isDebugEnabled) {
      val fromFiles = nodes.map(_.filename).mkString("[", ", ", "]")
      val nodeLabel = nodes.headOption.map(_.label).getOrElse("")
      logger.debug(s"Found ${nodes.size} duplicate $nodeLabel fullNames for '$fullName' in: $fromFiles")
    }
  }

}
