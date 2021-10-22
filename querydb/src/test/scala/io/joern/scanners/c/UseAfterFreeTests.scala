package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.console.scan._
import io.shiftleft.semanticcpg.language._

class UseAfterFreeTests extends CQueryTestSuite {
  override def queryBundle = UseAfterFree

   override val code =
    """
    |void good(a_struct_type *a_struct) {
    |
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
    |
    |""".stripMargin


  "should flag `bad` function only" in {
    val query = queryBundle.freeFieldNoReassign()
    val results = findMatchingCalls(query)

    results shouldBe Set("bad")
  }
}
