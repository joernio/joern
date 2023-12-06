// This test file has been translated from swift/test/Parse/async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class AsyncTests extends AbstractPassTest {

  "AsyncTests" should {

    "testAsync1" ignore AstFixture("""
       |func asyncGlobal1() async { }
       |func asyncGlobal2() async throws { }
       |""".stripMargin) { cpg => ??? }

    "testAsync2" ignore AstFixture("func asyncGlobal3() async throws { }") { cpg => ??? }

    "testAsync3" ignore AstFixture("func asyncGlobal3(fn: () throws -> Int) async rethrows { }") { cpg => ??? }

    "testAsync4" ignore AstFixture("func asyncGlobal4() async -> Int { }") { cpg => ??? }

    "testAsync6" ignore AstFixture("func asyncGlobal6() async throws -> Int { }") { cpg => ??? }

    "testAsync10a" ignore AstFixture("typealias AsyncFunc1 = () async -> ()") { cpg => ??? }

    "testAsync10b" ignore AstFixture("typealias AsyncFunc2 = () async throws -> ()") { cpg => ??? }

    "testAsync11a" ignore AstFixture("let _ = [() async -> ()]()") { cpg => ??? }

    "testAsync11b" ignore AstFixture("let _ = [() async throws -> ()]()") { cpg => ??? }

    "testAsync11d" ignore AstFixture("let _ = [() async -> ()]()") { cpg => ??? }

    "testAsync12" ignore AstFixture("""
      |struct MyFuture {
      |  func await() -> Int { 0 }
      |}""".stripMargin) { cpg => ??? }

    "testAsync13" ignore AstFixture("""
      |func testAwaitExpr() async {
      |  let _ = await asyncGlobal1()
      |  let myFuture = MyFuture()
      |  let _ = myFuture.await()
      |}""".stripMargin) { cpg => ??? }

    "testAsync14" ignore AstFixture("func getIntSomeday() async -> Int { 5 }") { cpg => ??? }

    "testAsync15" ignore AstFixture("""
      |func testAsyncLet() async {
      |  async let x = await getIntSomeday()
      |  _ = await x
      |}""".stripMargin) { cpg => ??? }

    "testAsync16" ignore AstFixture("async func asyncIncorrectly() { }") { cpg => ??? }

  }

}
