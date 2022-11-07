package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite

class FileOpRaceTests extends CQueryTestSuite(FileOpRace) {

  "should flag function `insecure_race` only" in {
    val query   = queryBundle.fileOperationRace()
    val results = findMatchingCalls(query)
    results shouldBe Set("insecure_race")
  }

}
