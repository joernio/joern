package io.joern.ghidra2cpg.passes.x86

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.X86Processor
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

class X86FunctionPass(currentProgram: Program,
                      filename: String,
                      function: Function,
                      cpg: Cpg,
                      keyPool: IntervalKeyPool,
                      decompInterface: DecompInterface)
    extends FunctionPass(new X86Processor, currentProgram, filename, function, cpg, keyPool, decompInterface) {
  override def runOnPart(part: String): Iterator[DiffGraph] = {
    createMethodNode()
    handleParameters()
    handleLocals()
    handleBody()
    Iterator(diffGraph.build())
  }

}
