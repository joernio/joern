package io.joern.pysrc2cpg

import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

object Py2Cpg {
  case class InputPair(content: String, relFileName: String)
  type InputProvider = () => InputPair
}

/** Entry point for general cpg generation from python code.
  *
  * @param inputProviders
  *   Set of functions which provide InputPairs. The functions must be safe to call from different threads.
  * @param outputCpg
  *   Empty target cpg which will be populated.
  * @param inputPath
  *   The project root.
  * @param requirementsTxt
  *   The configured name of the requirements txt file.
  * @param schemaValidationMode
  *   The boolean switch for enabling or disabling early schema checking during AST creation.
  */
class Py2Cpg(
  inputProviders: Iterable[Py2Cpg.InputProvider],
  outputCpg: Cpg,
  inputPath: String,
  requirementsTxt: String = "requirements.txt",
  schemaValidationMode: ValidationMode,
  enableFileContent: Boolean
) {
  private val diffGraph   = new DiffGraphBuilder()
  private val nodeBuilder = new NodeBuilder(diffGraph)
  private val edgeBuilder = new EdgeBuilder(diffGraph)

  def buildCpg(): Unit = {
    nodeBuilder.metaNode(Languages.PYTHONSRC, version = "").root(inputPath + java.io.File.separator)
    val globalNamespaceBlock =
      nodeBuilder.namespaceBlockNode(Constants.GLOBAL_NAMESPACE, Constants.GLOBAL_NAMESPACE, "N/A")
    nodeBuilder.typeNode(Constants.ANY, Constants.ANY)
    val anyTypeDecl =
      nodeBuilder.typeDeclNode(Constants.ANY, Constants.ANY, "N/A", Nil, LineAndColumn(1, 1, 1, 1, 1, 1))
    edgeBuilder.astEdge(anyTypeDecl, globalNamespaceBlock, 0)
    BatchedUpdate.applyDiff(outputCpg.graph, diffGraph)
    new CodeToCpg(outputCpg, inputProviders, schemaValidationMode, enableFileContent).createAndApply()
    new ConfigFileCreationPass(outputCpg, requirementsTxt).createAndApply()
    new DependenciesFromRequirementsTxtPass(outputCpg).createAndApply()
  }
}
