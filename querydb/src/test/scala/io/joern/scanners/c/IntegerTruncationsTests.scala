package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*
import io.joern.console.scan.*

class IntegerTruncationsTests extends CQueryTestSuite(IntegerTruncations) {

  "find truncation in assignment of `strlen` to `int`" in {
    queryBundle.strlenAssignmentTruncations()(cpg) match {
      case List(result) =>
        result.evidence.toList match {
          case List(x: nodes.Identifier) => x.method.name shouldBe "vulnerable"
          case _                         => fail()
        }
      case _ => fail()
    }
  }

}
