package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.shiftleft.semanticcpg.language._

class UseAfterFreePostUsage extends CQueryTestSuite(UseAfterFree) {

  "should flag functions `bad` and `false_positive` only" in {
    val x = queryBundle.freePostDominatesUsage()
    x(cpg)
      .flatMap(_.evidence)
      .cast[nodes.Identifier]
      .method
      .name
      .toSet shouldBe Set("bad", "false_positive")
  }

}
