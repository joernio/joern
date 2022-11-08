package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._

class NullTerminationTests extends CQueryTestSuite(NullTermination) {

  "should find the bad code and not report the good" in {
    val query = queryBundle.strncpyNoNullTerm()
    val results = query(cpg)
      .flatMap(_.evidence)
      .collect { case expr: nodes.Expression => expr }
      .method
      .name
      .toSetImmutable
    results shouldBe Set("bad")
  }

}
