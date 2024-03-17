// This test file has been translated from swift/test/Parse/async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class AsyncTests extends AstSwiftSrc2CpgSuite {

  "AsyncTests" should {

    "testAsync1" ignore {
      val cpg = code("""
       |func asyncGlobal1() async { }
       |func asyncGlobal2() async throws { }
       |""".stripMargin)
      ???
    }

    "testAsync2" ignore {
      val cpg = code("func asyncGlobal3() async throws { }")
      ???
    }

    "testAsync3" ignore {
      val cpg = code("func asyncGlobal3(fn: () throws -> Int) async rethrows { }")
      ???
    }

    "testAsync4" ignore {
      val cpg = code("func asyncGlobal4() async -> Int { }")
      ???
    }

    "testAsync6" ignore {
      val cpg = code("func asyncGlobal6() async throws -> Int { }")
      ???
    }

    "testAsync10a" ignore {
      val cpg = code("typealias AsyncFunc1 = () async -> ()")
      ???
    }

    "testAsync10b" ignore {
      val cpg = code("typealias AsyncFunc2 = () async throws -> ()")
      ???
    }

    "testAsync11a" ignore {
      val cpg = code("let _ = [() async -> ()]()")
      ???
    }

    "testAsync11b" ignore {
      val cpg = code("let _ = [() async throws -> ()]()")
      ???
    }

    "testAsync11d" ignore {
      val cpg = code("let _ = [() async -> ()]()")
      ???
    }

    "testAsync12" ignore {
      val cpg = code("""
      |struct MyFuture {
      |  func await() -> Int { 0 }
      |}""".stripMargin)
      ???
    }

    "testAsync13" ignore {
      val cpg = code("""
      |func testAwaitExpr() async {
      |  let _ = await asyncGlobal1()
      |  let myFuture = MyFuture()
      |  let _ = myFuture.await()
      |}""".stripMargin)
      ???
    }

    "testAsync14" ignore {
      val cpg = code("func getIntSomeday() async -> Int { 5 }")
      ???
    }

    "testAsync15" ignore {
      val cpg = code("""
      |func testAsyncLet() async {
      |  async let x = await getIntSomeday()
      |  _ = await x
      |}""".stripMargin)
      ???
    }

    "testAsync16" ignore {
      val cpg = code("async func asyncIncorrectly() { }")
      ???
    }

  }

}
