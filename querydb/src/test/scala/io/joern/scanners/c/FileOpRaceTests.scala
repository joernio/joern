package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.shiftleft.semanticcpg.language._

class FileOpRaceTests extends CQueryTestSuite {

  override def queryBundle = FileOpRace

  "should flag function `insecure_race` only" in {
    val query   = queryBundle.fileOperationRace()
    val results = findMatchingCalls(query)
    results shouldBe Set("insecure_race")
  }

}
