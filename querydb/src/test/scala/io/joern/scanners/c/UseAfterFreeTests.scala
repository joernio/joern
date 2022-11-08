package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.joern.x2cpg.testfixtures.TestCpg

class UseAfterFreeTests extends CQueryTestSuite(UseAfterFree) {

  override val cpg: TestCpg = code("""
    |void good(a_struct_type *a_struct) {
    |  free(a_struct->ptr);
    |  if (something) {
    |    a_struct->ptr = NULL;
    |    return;
    |  }
    |  a_struct->ptr = foo;
    |}
    |
    |void bad(a_struct_type *a_struct) {
    | free(a_struct->ptr);
    | if (something) {
    |   return;
    | }
    | a_struct->ptr = foo;
    |}
    |""".stripMargin)

  "should flag `bad` function only" in {
    val query   = queryBundle.freeFieldNoReassign()
    val results = findMatchingCalls(query)
    results shouldBe Set("bad")
  }
}
