// This test file has been translated from swift/test/Parse/pound_assert.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PoundAssertTests extends AbstractPassTest {

  "PoundAssertTests" should {

    "testPoundAssert1" ignore AstFixture("#assert(true, 123)") { cpg => ??? }

    "testPoundAssert2" ignore AstFixture(""" #assert(true, "error \(1) message")""") { cpg => ??? }

    "testPoundAssert5" ignore AstFixture("""
        |func unbalanced1() {
        |  #assert(true)
        |}
        |""".stripMargin) { cpg => ??? }

    "testPoundAssert6" ignore AstFixture("""
        |func unbalanced2() {
        |  #assert(true, "hello world")
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
