package io.joern.csharpsrc2cpg

import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase

import scala.util.Try

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {
  override def createCpg(config: Config): Try[Cpg] = Try(
    throw new NotImplementedError("CSharp2Cpg has not been implemented yet!")
  )

}

object CSharpSrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

}
