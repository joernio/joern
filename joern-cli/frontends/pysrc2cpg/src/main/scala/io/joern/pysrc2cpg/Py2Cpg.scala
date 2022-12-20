package io.joern.pysrc2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.pysrc2cpg.utils.FileOperations
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.IOUtils
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.util.Try

object Py2Cpg {
  case class InputPair(content: String, absFileName: String, relFileName: String)
  type InputProvider = () => InputPair
}

class Py2Cpg extends X2CpgFrontend[Config] {
    private val diffGraph   = new DiffGraphBuilder()
    private val nodeBuilder = new NodeBuilder(diffGraph)
    private val edgeBuilder = new EdgeBuilder(diffGraph)
    def createCpg(config: Config): Try[Cpg] = {
      withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
        // TODO maybe remove ignoredFiles from config since we are not using them
        val inputFiles = FileOperations.collectInputFiles(Paths.get(config.inputPath), Paths.get(config.venvDir).asScala.to(Iterable))
        val inputProviders = inputFiles.map { inputFile =>
          () => {
            val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
            Py2Cpg.InputPair(content, inputFile.toString, config.inputPath)
          }
        }

        nodeBuilder.metaNode(Languages.PYTHONSRC, version = "")
        val globalNamespaceBlock =
          nodeBuilder.namespaceBlockNode(Constants.GLOBAL_NAMESPACE, Constants.GLOBAL_NAMESPACE, "N/A")
        nodeBuilder.typeNode(Constants.ANY, Constants.ANY)
        val anyTypeDecl = nodeBuilder.typeDeclNode(Constants.ANY, Constants.ANY, "N/A", Nil, LineAndColumn(1, 1, 1, 1))
        edgeBuilder.astEdge(anyTypeDecl, globalNamespaceBlock, 0)
        BatchedUpdate.applyDiff(cpg.graph, diffGraph)
        new CodeToCpg(cpg, inputProviders).createAndApply()
        cpg
      }
  }

  def createCpgWithAllOverlays(config: Config): Try[Cpg] = {
    val maybeCpg = createCpgWithOverlays(config)
    maybeCpg.map { cpg =>
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
      cpg
    }
  }

}