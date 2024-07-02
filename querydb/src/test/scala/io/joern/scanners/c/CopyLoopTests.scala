package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*
import io.joern.console.scan.*

class CopyLoopTests extends CQueryTestSuite(CopyLoops) {

  "find indexed buffer assignment targets in loops where index is incremented" in {
    queryBundle.isCopyLoop()(cpg).map(_.evidence) match {
      case List(IndexedSeq(expr: nodes.Expression)) =>
        expr.method.name shouldBe "index_into_dst_array"
      case _ => fail()
    }
  }

}
