package io.joern.fuzzyc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.joern.fuzzyc2cpg.Global
import io.joern.fuzzyc2cpg.passes.astcreation.{AntlrCModuleParserDriver, AstVisitor}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import org.slf4j.LoggerFactory

/** Given a list of filenames, this pass creates abstract syntax trees for each file, including File and NamespaceBlock
  * Files are processed in parallel.
  */
class AstCreationPass(filenames: List[String], cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  val global: Global = Global()

  override def generateParts(): Array[String] = filenames.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {

    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val namespaceBlock = nodes
      .NewNamespaceBlock()
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath)))
      .filename(absolutePath)
      .order(1)

    diffGraph.addNode(namespaceBlock)
    val localDiff = new DiffGraphBuilder
    val driver    = createDriver(namespaceBlock)
    // only commit changes from within the file if the entire file succeeds
    if (tryToParse(driver, filename, localDiff)) diffGraph.absorb(localDiff)
  }

  private def createDriver(namespaceBlock: NewNamespaceBlock): AntlrCModuleParserDriver = {
    val driver     = new AntlrCModuleParserDriver()
    val astVisitor = new AstVisitor(driver, namespaceBlock, global)
    driver.addObserver(astVisitor)
    driver
  }

  private def tryToParse(driver: AntlrCModuleParserDriver, filename: String, diffGraph: DiffGraphBuilder): Boolean = {
    try {
      driver.parseAndWalkFile(filename, diffGraph)
      true
    } catch {
      case ex: Exception =>
        logger.warn("Cannot parse module: " + filename + ", skipping", ex)
        false
    }
  }

}
