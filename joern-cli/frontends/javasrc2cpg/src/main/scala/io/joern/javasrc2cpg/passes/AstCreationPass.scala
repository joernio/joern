package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.joern.javasrc2cpg.{Config, JpAstWithMeta}
import io.joern.x2cpg.datastructures.{CodeTree, Global, TreeNode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewType, NewTypeDecl, NewTypeParameter}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala

class AstCreationPass(asts: List[JpAstWithMeta], config: Config, cpg: Cpg, symbolSolver: JavaSymbolSolver)
    extends ConcurrentWriterCpgPass[JpAstWithMeta](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[JpAstWithMeta] = {
    logger.info(s"Found ${asts.size} files.")
    asts.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, astWithMeta: JpAstWithMeta): Unit = {
    val originalFilename = astWithMeta.fileInfo.originalFilename
    val result           = astWithMeta.compilationUnit
    diffGraph.absorb(new AstCreator(originalFilename, result, global, symbolSolver).createAst())
  }

}
