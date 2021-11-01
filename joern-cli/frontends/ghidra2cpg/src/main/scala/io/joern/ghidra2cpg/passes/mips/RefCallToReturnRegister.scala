package io.joern.ghidra2cpg.passes.mips

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import org.slf4j.{Logger, LoggerFactory}

// This pass adds REACHING_DEF edges from calls to specific procedures,
// into arguments of calls that move values from return registers v0 and v1.
class RefCallToReturnRegister(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def run(): Iterator[DiffGraph] = {
    logger.info("Running RefCallToReturnRegister...")
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

    def relevantCalls = cpg.call.code("getenv")
    val maxCfgPeekDistance = 6
    def instructionsAfterCalls =
      relevantCalls.map { c =>
        def cfgPeekForward =
          c.repeat(_.cfgNext)(_.emit(_.isCall).times(maxCfgPeekDistance))
        def storeFromReturnRegister =
          cfgPeekForward.isCall
            .where(_.argument(1).code("v0|v1"))
            .code("sw.*") // TODO: maybe match on operator instead
            .argument(2)
        (c, storeFromReturnRegister.l)
      }
    instructionsAfterCalls.foreach { callToArgs =>
      callToArgs._2.foreach { arg =>
        diffGraph.addEdge(callToArgs._1, arg, EdgeTypes.REACHING_DEF, Seq((PropertyNames.VARIABLE, arg.code)))
      }
    }
    Iterator(diffGraph.build())
  }
}
