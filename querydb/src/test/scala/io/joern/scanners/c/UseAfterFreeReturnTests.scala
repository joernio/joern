package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan.*
import io.shiftleft.semanticcpg.language.*

class UseAfterFreeReturnTests extends CQueryTestSuite(UseAfterFree) {

  "should flag `bad` function only" in {
    val x = queryBundle.freeReturnedValue()
    x(cpg)
      .flatMap(_.evidence)
      .cast[nodes.Identifier]
      .method
      .name
      .toSet shouldBe Set("bad")
  }

}
