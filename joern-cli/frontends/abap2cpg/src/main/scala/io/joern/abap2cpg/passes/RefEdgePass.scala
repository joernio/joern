package io.joern.abap2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

/** Creates REF edges from IDENTIFIER nodes to their declarations (LOCAL, PARAMETER, etc.)
  *
  * This is required for dataflow analysis to work correctly.
  */
class RefEdgePass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger = org.slf4j.LoggerFactory.getLogger(classOf[RefEdgePass])

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    var totalEdges       = 0
    var totalIdentifiers = 0
    var totalDecls       = 0
    var methodCount      = 0

    logger.info(s"RefEdgePass: Starting with ${cpg.method.size} methods")

    // For each method, resolve identifiers to their declarations
    cpg.method.foreach { method =>
      methodCount += 1

      // Build a map of variable names to their declaration nodes within this method's scope
      val localDecls = method.local.map(local => local.name -> local).toMap
      val paramDecls = method.parameter.map(param => param.name -> param).toMap

      // Combine all declarations
      val allDecls = localDecls ++ paramDecls
      totalDecls += allDecls.size

      if (methodCount <= 3) {
        logger.info(s"Method ${method.name}: ${localDecls.size} locals, ${paramDecls.size} params")
      }

      // Find all identifiers in this method
      val identifiers = method.ast.isIdentifier.l
      if (methodCount <= 3) {
        logger.info(s"Method ${method.name}: ${identifiers.size} identifiers in AST")
      }

      identifiers.foreach { identifier =>
        totalIdentifiers += 1
        // Try to resolve the identifier to a declaration
        allDecls.get(identifier.name).foreach { declNode =>
          // Add REF edge from identifier to its declaration
          diffGraph.addEdge(identifier, declNode, EdgeTypes.REF)
          totalEdges += 1
          if (totalEdges <= 5) {
            logger.info(s"Creating REF: ${identifier.name} -> ${declNode.label}")
          }
        }
      }
    }

    logger.info(
      s"RefEdgePass: Created $totalEdges REF edges for $totalIdentifiers identifiers (${totalDecls} declarations in ${methodCount} methods)"
    )
  }
}
