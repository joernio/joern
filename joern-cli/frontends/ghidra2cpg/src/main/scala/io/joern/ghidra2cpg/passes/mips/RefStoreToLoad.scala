package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import org.slf4j.{Logger, LoggerFactory}

// This pass adds REACHING_DEF edges between consecutive ARGUMENT nodes of CALLs
// which represent memory addresses of store/load instructions.
class RefStoreToLoad(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def run(): Iterator[DiffGraph] = {
    logger.info("Running RefStoreToLoad...")
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

    val maxCfgPeekDistance = 8
    def consecutiveStoreLoadPairs =
      cpg.call.flatMap { c =>
        if (!c.code.startsWith("sw")) {
          None
        } else {
          def loadFromMatchingMemoryAddress =
            c.cfgNext(maxCfgPeekDistance).isCall
              .filter(_.code.startsWith("lw"))
              .where(_.argument(2).codeExact(c.argument(2).code))
          Some((c, loadFromMatchingMemoryAddress.l))
        }
      }

    consecutiveStoreLoadPairs.l.foreach { pair =>
      pair._2.foreach { loadWordCall =>
        logger.info(
          "Adding REACHING_DEF edge from second argument of `" + pair._1.code + "` to second argument of `" + loadWordCall.code + "`.")
        diffGraph.addEdge(pair._1.argument(2),
                          loadWordCall.argument(2),
                          EdgeTypes.REACHING_DEF,
                          Seq((PropertyNames.VARIABLE, loadWordCall.code)))
      }
    }
    Iterator(diffGraph.build())
  }
}
