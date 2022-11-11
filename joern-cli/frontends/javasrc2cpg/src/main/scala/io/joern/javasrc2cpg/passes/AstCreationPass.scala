package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.javasrc2cpg.{Config, JpAstWithMeta}
import io.joern.x2cpg.datastructures.Global
import org.slf4j.LoggerFactory

import java.util.concurrent.atomic.AtomicInteger

class AstCreationPass(asts: List[JpAstWithMeta], config: Config, cpg: Cpg, symbolSolver: JavaSymbolSolver)
    extends ConcurrentWriterCpgPass[JpAstWithMeta](cpg) {

  val global: Global   = new Global()
  private val logger   = LoggerFactory.getLogger(classOf[AstCreationPass])
  private val finished = new AtomicInteger(0)
  private val initTime = System.nanoTime()

  override def generateParts(): Array[JpAstWithMeta] = {
    logger.info(s"Found ${asts.size} files.")
    asts.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, astWithMeta: JpAstWithMeta): Unit = {
    val originalFilename = astWithMeta.fileInfo.originalFilename
    val result           = astWithMeta.compilationUnit
    diffGraph.absorb(new AstCreator(originalFilename, result, global, symbolSolver).createAst())

    if (finished.addAndGet(1) % 250 == 0) {
      val nowTime = System.nanoTime()
      logger.info(s"Finished files: $finished")
      logger.info(s"Time elapsed  : ${(nowTime - initTime) / 1000000000.0}")
    }
  }

}
