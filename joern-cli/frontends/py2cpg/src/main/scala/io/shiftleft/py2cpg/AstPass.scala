package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class AstPass(cpg: Cpg,
              sourceFiles: Iterable[String],
              keyPool: IntervalKeyPool)
  extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(sourceFiles.size))) {

  override def partIterator: Iterator[String] = sourceFiles.iterator

  override def runOnPart(sourceFile: String): Iterator[DiffGraph] = {
    Iterator.empty
  }
}
