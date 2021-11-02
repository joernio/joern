package io.joern.ghidra2cpg.passes.arm

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.ArmProcessor
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

class ArmFunctionPass(currentProgram: Program,
                      filename: String,
                      function: Function,
                      cpg: Cpg,
                      keyPool: IntervalKeyPool,
                      decompInterface: DecompInterface)
    extends FunctionPass(new ArmProcessor, currentProgram, filename, function, cpg, keyPool, decompInterface) {

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    createMethodNode()
    handleParameters()
    handleLocals()
    handleBody()
    Iterator(diffGraph.build())
  }
}
