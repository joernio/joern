package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan.*
import io.shiftleft.semanticcpg.language.*

class HeapBasedOverflowTests extends CQueryTestSuite(HeapBasedOverflow) {

  "find calls to malloc/memcpy system with different expressions in arguments" in {
    val x = queryBundle.mallocMemcpyIntOverflow()
    x(cpg).map(_.evidence) match {
      case List(IndexedSeq(expr: nodes.Expression)) =>
        expr.code shouldBe "memcpy(dst, src, len + 7)"
      case _ => fail()
    }
  }

}
