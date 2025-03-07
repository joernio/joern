package io.joern.c2cpg.passes

import io.joern.c2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

/** Ensures that method fullNames are unique across the CPG. In C and C ++ code, function names can appear multiple
  * times across different translation units, particularly with static functions in different files sharing the same
  * name. Sometimes we also may fail to resolve proper type names (missing includes, missing macro expanding) and that,
  * ultimately, leads to the same function fullName being generated.
  *
  * The CPG specification requires unique fullNames for all kind of linking and querying operations.
  *
  * This unique fullName generation cannot be performed during the initial AST traversal because:
  *   - The AST is built on a per-file basis where we don't have knowledge of methods defined in other files
  *   - Name uniqueness must be established across the entire codebase, not just within a single file.
  *   - Using a global cache during parsing would slow down our parallel processing.
  *   - Simply prepending the filename to the method name does not work as we can not determine that fullName for the
  *     call (leads to linking issues later on). Also, the same function fullName may be generated multiple times in on
  *     file due to missing type information (as mentioned above).
  *
  * Instead , this pass runs after all methods have been added to the CPG, identifies duplicates, and appends unique
  * suffixes to ensure each method can be uniquely identified. Calls to static methods affected by this are also updated
  * to reflect the new method fullName.
  */
class MethodFullNameUniquenessPass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(classOf[MethodFullNameUniquenessPass])

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val methodFullNameMap = cpg.method
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .groupBy(_.fullName)
      .filter(_._2.size > 1)
      // sort for stable output
      .sortBy(_._1)
    methodFullNameMap.foreach { case (fullName, methods) =>
      val fromFiles = methods.map(_.filename).mkString("[", ", ", "]")
      logger.debug(s"Found ${methods.size} duplicate method fullNames for '$fullName' in: $fromFiles")
      val sortedMethods = methods.sortBy { method =>
        val fileName = method.filename
        val lineNr   = method.lineNumber.getOrElse(-1)
        val columnNr = method.columnNumber.getOrElse(-1)
        s"$fileName:$lineNr:$columnNr"
      }
      lazy val callsAffected =
        cpg.call.methodFullNameExact(fullName).filterNot(_.file.exists(_.name == sortedMethods.head.filename)).l
      sortedMethods.tail.zipWithIndex.foreach { case (method, index) =>
        val signature   = method.signature
        val isCFunction = !fullName.endsWith(s":$signature")
        val newFullName = if (isCFunction) {
          s"$fullName${Defines.DuplicateSuffix}$index"
        } else {
          val fullNameWithoutSignature = fullName.stripSuffix(s":$signature")
          s"$fullNameWithoutSignature${Defines.DuplicateSuffix}$index:$signature"
        }
        // set the new fullName
        dstGraph.setNodeProperty(method, Method.PropertyNames.FullName, newFullName)
        // fixup calls to static methods in the same compilation unit via the naive namespace-by-filename approach
        if (method.isStatic.nonEmpty) {
          val callCandidates = callsAffected.filter(_.file.exists(_.name == method.filename))
          // set the new methodFullName
          callCandidates.foreach { call =>
            dstGraph.setNodeProperty(call, Call.PropertyNames.MethodFullName, newFullName)
          }
        }
      }
    }
  }

}
