// This test file has been translated from swift/test/Parse/async-syntax.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class AsyncSyntaxTests extends AbstractPassTest {

  "AsyncSyntaxTests" should {

    "testAsyncSyntax1" ignore AstFixture("""
        |func asyncGlobal1() async { }
        |func asyncGlobal2() async throws { }
        |""".stripMargin) { cpg => ??? }

    "testAsyncSyntax2" ignore AstFixture("""
        |typealias AsyncFunc1 = () async -> ()
        |typealias AsyncFunc2 = () async throws -> ()
        |typealias AsyncFunc3 = (_ a: Bool, _ b: Bool) async throws -> ()
        |""".stripMargin) { cpg => ??? }

    "testAsyncSyntax3" ignore AstFixture("""
        |let _ = [() async -> ()]()
        |let _ = [() async throws -> ()]()""".stripMargin) { cpg => ??? }

    "testAsyncSyntax4" ignore AstFixture("let _ = await asyncGlobal1()") { cpg => ??? }

    "testAsyncSyntax5" ignore AstFixture("""
        |let _ = { () async in 5 }
        |let _ = { () throws in 5 }
        |let _ = { () async throws in 5 }""".stripMargin) { cpg => ??? }

    "testAsyncSyntax6" ignore AstFixture("""
        |func testAwait() async {
        |  let _ = await asyncGlobal1()
        |}""".stripMargin) { cpg => ??? }
  }

}
