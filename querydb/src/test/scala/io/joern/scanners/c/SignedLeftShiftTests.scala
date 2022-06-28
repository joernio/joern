package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.shiftleft.semanticcpg.language._

class SignedLeftShiftTests extends CQueryTestSuite {

  override def queryBundle = SignedLeftShift

  "find signed left shift" in {
    val query   = queryBundle.signedLeftShift()
    val results = findMatchingCalls(query)
    results shouldBe Set("bad1", "bad2", "bad3")
  }

}
