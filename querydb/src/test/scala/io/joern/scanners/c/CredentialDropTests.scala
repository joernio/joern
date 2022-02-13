package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class CredentialDropTests extends CQueryTestSuite {

  override def queryBundle = CredentialDrop

  "find cases where user changes are not preceded by calls to set*gid and setgroups" in {
    val query   = queryBundle.userCredDrop()
    val results = findMatchingCalls(query)

    results shouldBe Set("bad1", "bad3")
  }

  "find cases where group membership changes are not preceded by a call to setgroups" in {
    val query   = queryBundle.groupCredDrop()
    val results = findMatchingCalls(query)

    results shouldBe Set("bad2")
  }
}
