package io.joern.scanners.ghidra

import io.joern.suites.GhidraQueryTestSuite

class DangerousFunctionsTests extends GhidraQueryTestSuite {
  override def queryBundle = DangerousFunctions

  "find insecure strcpy" in {
    buildCpgForBin("dangerous_functions.o")

    val query   = queryBundle.strcpyUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("vulnerable_strcpy")
  }

}
