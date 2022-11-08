package io.joern.scanners.ghidra

import io.joern.suites.GhidraQueryTestSuite

class UserInputIntoDangerousFunctionsTests extends GhidraQueryTestSuite(UserInputIntoDangerousFunctions) {

  "getenvToStrcpy query" when {
    def query = queryBundle.getenvToStrcpy()

    "executed on CPG for binary call to `strcpy`, but no call to `getenv`" should {
      "return an empty set of matched method names" in {
        buildCpgForBin("buf1.exe")
        val results = methodNamesForMatchedPoints(query)
        results shouldBe Set()
      }
    }

    "executed on CPG for binary call to `strcpy`, and call to `getenv`, but no dataflow between them" should {
      "return an empty set of matched method names" in {
        buildCpgForBin("buf2_neg.exe")
        val results = methodNamesForMatchedPoints(query)
        results shouldBe Set()
      }
    }

    "executed on CPG for binary with dataflow between `getenv` return value and `strcpy` source argument" should {
      "find main function with data flow between getenv and strcpy" in {
        buildCpgForBin("buf2.exe")
        val results = methodNamesForMatchedPoints(query)
        results shouldBe Set("main")
      }
    }
  }
}
