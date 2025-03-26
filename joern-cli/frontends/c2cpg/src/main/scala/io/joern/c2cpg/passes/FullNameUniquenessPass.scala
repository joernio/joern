package io.joern.c2cpg.passes

import io.joern.c2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Binding
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes.NamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

/** Ensures that method, type decl, and namespace block fullNames are unique across the CPG. In C and C ++ code, these
  * elements can appear multiple times across different translation units, e.g., with static functions in different
  * files sharing the same name, classes declared and defined across different header and implementation files, or
  * namespaces spanning different files. Sometimes we also may fail to resolve proper type names (missing includes,
  * missing macro expanding) and that, ultimately, leads to the same fullName being generated.
  *
  * The CPG specification requires unique fullNames for all kind of linking and querying operations.
  *
  * This unique fullName generation cannot be performed during the initial AST traversal because:
  *   - The AST is built on a per-file basis where we don't have knowledge of methods defined in other files
  *   - Name uniqueness must be established across the entire codebase, not just within a single file.
  *   - Using a global cache during parsing would slow down our parallel processing.
  *   - Simply prepending the filename to the fullName does not work as we can not determine that fullName for all calls
  *     (leads to linking issues later on) or referenced type. Also, the same fullName may be generated multiple times
  *     in on file due to missing type information (as mentioned above).
  *
  * Instead , this pass runs after all methods, type decls, and namespaces have been added to the CPG, identifies
  * duplicates, and appends unique suffixes to ensure each one can be uniquely identified. Calls to static methods
  * affected by this are also updated to reflect the new method fullName.
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
      lazy val bindingsAffected = cpg.binding.methodFullNameExact(fullName).l
      sortedMethods.tail.zipWithIndex.foreach { case (method, index) =>
        val suffix      = s"${Defines.DuplicateSuffix}$index"
        val signature   = method.signature
        val isCFunction = !fullName.endsWith(s":$signature")
        val newFullName = if (isCFunction) {
          s"$fullName$suffix"
        } else {
          val fullNameWithoutSignature = fullName.stripSuffix(s":$signature")
          s"$fullNameWithoutSignature$suffix:$signature"
        }
        dstGraph.setNodeProperty(method, Method.PropertyNames.FullName, newFullName)
        // fixup bindings
        bindingsAffected.filter(_.refOut.contains(method)).foreach { binding =>
          dstGraph.setNodeProperty(binding, Binding.PropertyNames.Name, s"${binding.name}$suffix")
          dstGraph.setNodeProperty(binding, Binding.PropertyNames.MethodFullName, newFullName)
        }
        // fixup calls to static methods in the same compilation unit via the naive namespace-by-filename approach
        if (method.isStatic.nonEmpty) {
          val callCandidates = callsAffected.filter(_.file.exists(_.name == method.filename))
          callCandidates.foreach { call =>
            dstGraph.setNodeProperty(call, Call.PropertyNames.Name, s"${call.name}$suffix")
            dstGraph.setNodeProperty(call, Call.PropertyNames.MethodFullName, newFullName)
          }
        }
      }
    }
  }

  private def generateStableFullNameMapping[T <: AffectedNodeType](nodes: Iterator[T]): Seq[(String, List[T])] = {
    nodes
      .groupBy(_.fullName)
      .filter(_._2.size > 1)
      // sort for stable output
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

  private def handleTypeDecls(dstGraph: DiffGraphBuilder): Unit = {
    handleDuplicateFullNames(
      cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName),
      TypeDecl.PropertyNames.FullName,
      dstGraph
    )
  }

  private def handleNamespaceBlocks(dstGraph: DiffGraphBuilder): Unit = {
    handleDuplicateFullNames(
      cpg.namespaceBlock.nameNot(NamespaceTraversal.globalNamespaceName),
      NamespaceBlock.PropertyNames.FullName,
      dstGraph
    )
  }

  private def handleDuplicateFullNames[T <: AffectedNodeType](
    nodes: Iterator[T],
    fullNameProperty: String,
    dstGraph: DiffGraphBuilder
  ): Unit = {
    val fullNameMap = generateStableFullNameMapping(nodes)
    fullNameMap.foreach { case (fullName, nodesList) =>
      logDuplicates(nodesList, fullName)
      val sortedNodes = sortNodesByLocation(nodesList)
      sortedNodes.tail.zipWithIndex.foreach { case (node, index) =>
        val suffix      = s"${Defines.DuplicateSuffix}$index"
        val newFullName = s"${node.fullName}$suffix"
        dstGraph.setNodeProperty(node, fullNameProperty, newFullName)
      }
    }
  }

}
