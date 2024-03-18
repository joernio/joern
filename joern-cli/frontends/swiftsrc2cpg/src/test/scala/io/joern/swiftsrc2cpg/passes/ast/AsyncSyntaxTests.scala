// This test file has been translated from swift/test/Parse/async-syntax.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class AsyncSyntaxTests extends AstSwiftSrc2CpgSuite {

  "AsyncSyntaxTests" should {

    "testAsyncSyntax1" ignore {
      val cpg = code("""
        |func asyncGlobal1() async { }
        |func asyncGlobal2() async throws { }
        |""".stripMargin)
      ???
    }

    "testAsyncSyntax2" ignore {
      val cpg = code("""
        |typealias AsyncFunc1 = () async -> ()
        |typealias AsyncFunc2 = () async throws -> ()
        |typealias AsyncFunc3 = (_ a: Bool, _ b: Bool) async throws -> ()
        |""".stripMargin)
      ???
    }

    "testAsyncSyntax3" ignore {
      val cpg = code("""
        |let _ = [() async -> ()]()
        |let _ = [() async throws -> ()]()""".stripMargin)
      ???
    }

    "testAsyncSyntax4" ignore {
      val cpg = code("let _ = await asyncGlobal1()")
      ???
    }

    "testAsyncSyntax5" ignore {
      val cpg = code("""
        |let _ = { () async in 5 }
        |let _ = { () throws in 5 }
        |let _ = { () async throws in 5 }""".stripMargin)
      ???
    }

    "testAsyncSyntax6" ignore {
      val cpg = code("""
        |func testAwait() async {
        |  let _ = await asyncGlobal1()
        |}""".stripMargin)
      ???
    }
  }

}
