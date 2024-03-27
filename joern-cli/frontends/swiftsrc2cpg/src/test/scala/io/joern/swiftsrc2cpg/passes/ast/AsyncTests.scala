// This test file has been translated from swift/test/Parse/async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class AsyncTests extends AstSwiftSrc2CpgSuite {

  "AsyncTests" should {

    "testAsync1" in {
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

    "testAsync3" in {
      val cpg                = code("func asyncGlobal3(fn: () throws -> Int) async rethrows { }")
      val List(asyncGlobal3) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      asyncGlobal3.name shouldBe "asyncGlobal3"
      asyncGlobal3.fullName shouldBe "Test0.swift:<global>:asyncGlobal3:ANY(() throws -> Int)"
      val List(fn) = asyncGlobal3.parameter.l
      fn.name shouldBe "fn"
      fn.typeFullName shouldBe "Test0.swift:<global>:asyncGlobal3:<anon-class>0"
      val List(t) = cpg.typeDecl.fullNameExact("Test0.swift:<global>:asyncGlobal3:<anon-class>0").l
      t.code shouldBe "() throws -> Int"
    }

    "testAsync4" in {
      val cpg                = code("func asyncGlobal4() async -> Int { }")
      val List(asyncGlobal4) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      asyncGlobal4.name shouldBe "asyncGlobal4"
      asyncGlobal4.fullName shouldBe "Test0.swift:<global>:asyncGlobal4:Int()"
      asyncGlobal4.methodReturn.typeFullName shouldBe "Int"
    }

    "testAsync6" in {
      val cpg                = code("func asyncGlobal6() async throws -> Int { }")
      val List(asyncGlobal6) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      asyncGlobal6.name shouldBe "asyncGlobal6"
      asyncGlobal6.fullName shouldBe "Test0.swift:<global>:asyncGlobal6:Int()"
      asyncGlobal6.methodReturn.typeFullName shouldBe "Int"
    }

    "testAsync10a" in {
      val cpg      = code("typealias AsyncFunc1 = () async -> ()")
      val List(f1) = cpg.typeDecl.where(_.aliasTypeFullName).l
      f1.name shouldBe "AsyncFunc1"
      f1.fullName shouldBe "Test0.swift:<global>:AsyncFunc1"
      f1.aliasTypeFullName shouldBe Option("Test0.swift:<global>:<anon-class>0")
      cpg.typeDecl.fullNameExact("Test0.swift:<global>:<anon-class>0").size shouldBe 1
    }

    "testAsync10b" in {
      val cpg      = code("typealias AsyncFunc2 = () async throws -> ()")
      val List(f2) = cpg.typeDecl.where(_.aliasTypeFullName).l
      f2.name shouldBe "AsyncFunc2"
      f2.fullName shouldBe "Test0.swift:<global>:AsyncFunc2"
      f2.aliasTypeFullName shouldBe Option("Test0.swift:<global>:<anon-class>0")
      cpg.typeDecl.fullNameExact("Test0.swift:<global>:<anon-class>0").size shouldBe 1
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

    "testAsync12" in {
      val cpg = code("""
      |struct MyFuture {
      |  func await() -> Int { 0 }
      |}""".stripMargin)
      val List(struct) = cpg.typeDecl.nameExact("MyFuture").l
      val List(await)  = struct.boundMethod.l
      await.name shouldBe "await"
      await.fullName shouldBe "Test0.swift:<global>:MyFuture:await:Int()"
      await.methodReturn.typeFullName shouldBe "Int"
    }

    "testAsync13" in {
      val cpg = code("""
      |func testAwaitExpr() async {
      |  let _ = await asyncGlobal1()
      |  let myFuture = MyFuture()
      |  let _ = myFuture.await()
      |}""".stripMargin)
      cpg.method.nameExact("testAwaitExpr").call.code.sorted.l shouldBe List(
        "MyFuture()",
        "asyncGlobal1()",
        "await asyncGlobal1()",
        "let myFuture = MyFuture()",
        "let wildcard_0 = await asyncGlobal1()",
        "let wildcard_1 = myFuture.await()",
        "myFuture.await",
        "myFuture.await()"
      )
    }

    "testAsync14" in {
      val cpg                 = code("func getIntSomeday() async -> Int { 5 }")
      val List(getIntSomeday) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      getIntSomeday.name shouldBe "getIntSomeday"
      getIntSomeday.fullName shouldBe "Test0.swift:<global>:getIntSomeday:Int()"
      getIntSomeday.methodReturn.typeFullName shouldBe "Int"
    }

    "testAsync15" in {
      val cpg = code("""
      |func testAsyncLet() async {
      |  async let x = await getIntSomeday()
      |  _ = await x
      |}""".stripMargin)
      cpg.method.nameExact("testAsyncLet").call.code.sorted.l shouldBe List(
        "_ = await x",
        "await getIntSomeday()",
        "await x",
        "getIntSomeday()",
        "let x = await getIntSomeday()"
      )
    }

    "testAsync16" in {
      val cpg                    = code("async func asyncIncorrectly() { }")
      val List(asyncIncorrectly) = cpg.method.internal.nameNot(NamespaceTraversal.globalNamespaceName).l
      asyncIncorrectly.name shouldBe "asyncIncorrectly"
      asyncIncorrectly.fullName shouldBe "Test0.swift:<global>:asyncIncorrectly:ANY()"
    }

  }

}
