package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite

class SignedLeftShiftTests extends CQueryTestSuite(SignedLeftShift) {

  "find signed left shift" in {
    val query   = queryBundle.signedLeftShift()
    val results = findMatchingCalls(query)
    results shouldBe Set("bad1", "bad2", "bad3")
  }

}
