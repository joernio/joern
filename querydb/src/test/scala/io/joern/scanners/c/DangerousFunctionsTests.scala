package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.Query
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._

class DangerousFunctionsTests extends CQueryTestSuite {

  override def queryBundle = DangerousFunctions

  "find insecure gets() function usage" in {
    val query   = queryBundle.getsUsed()
    val results = findMatchingCalls(query)

    results shouldBe (Set("insecure_gets"))
  }

  "find insecure printf() function usage" in {
    val query   = queryBundle.argvUsedInPrintf()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_sprintf", "insecure_printf")
  }

  "find insecure scanf() function usage" in {
    val query   = queryBundle.scanfUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_scanf")
  }

  "find insecure strncat() function usage" in {
    val query   = queryBundle.strcatUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_strcat", "insecure_strncat")
  }

  "find insecure strncpy() function usage" in {
    val query   = queryBundle.strcpyUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_strcpy", "insecure_strncpy")
  }

  "find insecure strtok() function usage" in {
    val query   = queryBundle.strtokUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_strtok")
  }

  "find insecure getwd() function usage" in {
    val query   = queryBundle.getwdUsed()
    val results = findMatchingCalls(query)

    results shouldBe Set("insecure_getwd")
  }
}
