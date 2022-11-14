package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite

class RetvalChecksTests extends CQueryTestSuite(RetvalChecks) {

  "should find unchecked read and not flag others" in {
    val query   = queryBundle.uncheckedReadRecvMalloc()
    val results = findMatchingCalls(query)
    results shouldBe Set("unchecked_read", "checks_something_else")
  }

}
