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

  private var hugeMethodInfo: Option[(methodCount: Int, largestMethod: (size: Int, fullName: String, file: String))] =
    None

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val sizeBefore = diffGraph.size
    new CfgCreator(method, diffGraph).run()
    val sizeOfCfg = diffGraph.size - sizeBefore
    if (sizeOfCfg > 100 * 1000) {
      val thisMethod = (sizeOfCfg, method.fullName, method.filename)
      synchronized {
        hugeMethodInfo = hugeMethodInfo match {
          case Some((count, largestMethod)) => Some((count + 1, Ordering.Tuple3.max(thisMethod, largestMethod)))
          case None                         => Some((1, thisMethod))
        }
      }
    }
  }

  override def finish(): Unit = {
    hugeMethodInfo.foreach { case (methodCount, largestMethod) =>
      logger.warn(
        "{} methods have a huge CFG with over 100 000 edges. The largest method {} from file {} has {} CFG edges. Analysis may benefit from excluding the containing file(s).",
        methodCount,
        largestMethod.fullName,
        largestMethod.file,
        largestMethod.size,
      )
    }
  }

}
