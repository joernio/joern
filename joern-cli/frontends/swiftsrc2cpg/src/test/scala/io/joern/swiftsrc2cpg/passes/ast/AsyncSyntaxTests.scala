// This test file has been translated from swift/test/Parse/async-syntax.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class AsyncSyntaxTests extends AstSwiftSrc2CpgSuite {

  "AsyncSyntaxTests" should {

    "testAsyncSyntax1" in {
      val cpg = code("""
        |func asyncGlobal1() async { }
        |func asyncGlobal2() async throws { }
        |""".stripMargin)
      val List(asyncGlobal1, asyncGlobal2) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      asyncGlobal1.name shouldBe "asyncGlobal1"
      asyncGlobal1.fullName shouldBe "Test0.swift:<global>:asyncGlobal1:ANY()"
      asyncGlobal2.name shouldBe "asyncGlobal2"
      asyncGlobal2.fullName shouldBe "Test0.swift:<global>:asyncGlobal2:ANY()"
    }

    "testAsyncSyntax2" in {
      val cpg = code("""
        |typealias AsyncFunc1 = () async -> ()
        |typealias AsyncFunc2 = () async throws -> ()
        |typealias AsyncFunc3 = (_ a: Bool, _ b: Bool) async throws -> ()
        |""".stripMargin)
      val List(f1, f2, f3) = cpg.typeDecl.where(_.aliasTypeFullName).l
      f1.name shouldBe "AsyncFunc1"
      f1.fullName shouldBe "Test0.swift:<global>:AsyncFunc1"
      f1.aliasTypeFullName shouldBe Option("Test0.swift:<global>:<anon-class>0")
      cpg.typeDecl.fullNameExact("Test0.swift:<global>:<anon-class>0").size shouldBe 1

      f2.name shouldBe "AsyncFunc2"
      f2.fullName shouldBe "Test0.swift:<global>:AsyncFunc2"
      f2.aliasTypeFullName shouldBe Option("Test0.swift:<global>:<anon-class>1")
      cpg.typeDecl.fullNameExact("Test0.swift:<global>:<anon-class>1").size shouldBe 1

      f3.name shouldBe "AsyncFunc3"
      f3.fullName shouldBe "Test0.swift:<global>:AsyncFunc3"
      f3.aliasTypeFullName shouldBe Option("Test0.swift:<global>:<anon-class>2")
      cpg.typeDecl.fullNameExact("Test0.swift:<global>:<anon-class>2").size shouldBe 1
    }

    "testAsyncSyntax3" ignore {
      val cpg = code("""
        |let _ = [() async -> ()]()
        |let _ = [() async throws -> ()]()""".stripMargin)
      ???
    }

    "testAsyncSyntax4" in {
      val cpg             = code("let _ = await asyncGlobal1()")
      val List(awaitCall) = cpg.call.nameExact("<operator>.await").l
      val List(call)      = awaitCall.argument.isCall.l
      call.name shouldBe "asyncGlobal1"
    }

    "testAsyncSyntax5" in {
      val cpg = code("""
        |let _ = { () async in 5 }
        |let _ = { () throws in 5 }
        |let _ = { () async throws in 5 }""".stripMargin)
      val List(f1, f2, f3) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      f1.name shouldBe "<lambda>0"
      f1.fullName shouldBe "Test0.swift:<global>:<lambda>0:ANY()"
      f2.name shouldBe "<lambda>1"
      f2.fullName shouldBe "Test0.swift:<global>:<lambda>1:ANY()"
      f3.name shouldBe "<lambda>2"
      f3.fullName shouldBe "Test0.swift:<global>:<lambda>2:ANY()"
    }

    "testAsyncSyntax6" ignore {
      val cpg = code("""
        |func testAwait() async {
        |  let _ = await asyncGlobal1()
        |}""".stripMargin)
      val List(f)         = cpg.method.nameExact("testAwait").l
      val List(awaitCall) = f.call.nameExact("<operator>.await").l
      val List(call)      = awaitCall.argument.isCall.l
      call.name shouldBe "asyncGlobal1"
    }
  }

}
