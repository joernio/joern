package io.joern.x2cpg.passes.controlflow

import io.joern.x2cpg.passes.controlflow.CfgCreationPass.logger
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object CfgCreationPass {
  private val logger = LoggerFactory.getLogger(classOf[CfgCreationPass])
}

/** A pass that creates control flow graphs from abstract syntax trees.
  *
  * Control flow graphs can be calculated independently per method. Therefore, we inherit from
  * `ForkJoinParallelCpgPass`.
  */
class CfgCreationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  private val hugeMethods = mutable.Buffer[(size: Int, method: String, file: String)]()

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val sizeBefore = diffGraph.size
    new CfgCreator(method, diffGraph).run()
    val sizeOfCfg = diffGraph.size - sizeBefore
    if (sizeOfCfg > 100 * 1000) {
      hugeMethods.synchronized {
        hugeMethods.append((sizeOfCfg, method.fullName, method.filename))
      }
    }
  }

  override def finish(): Unit = {
    if (hugeMethods.nonEmpty) {
      val max = hugeMethods.max

      logger.warn(
        "{} methods have a huge CFG with over 100 000 edges. The largest method {} from file {} has {} CFG edges. Analysis may benefit from excluding the containing file(s).",
        hugeMethods.size,
        max.method,
        max.file,
        max.size,
      )
    }
  }

}
