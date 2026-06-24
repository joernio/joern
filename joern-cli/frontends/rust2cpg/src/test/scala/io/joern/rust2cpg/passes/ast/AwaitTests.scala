package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.astcreation.RustOperators
import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class AwaitTests extends Rust2CpgSuite(noSysRoot = true) {

  "an `async fn`" should {
    val cpg = code("""
        |async fn f() -> i32 { 1 }
        |async fn g() {}
        |""".stripMargin)

    "type the METHOD_RETURN as i32" in {
      cpg.method.name("f").methodReturn.typeFullName.l shouldBe List("i32")
    }

    "type the METHOD_RETURN as ()" in {
      cpg.method.name("g").methodReturn.typeFullName.l shouldBe List("()")
    }
  }

  "an `await` expression" should {
    val cpg = code("""
        |async fn f() -> i32 { 1 }
        |async fn g() {
        | let x = f().await;
        |}
        |""".stripMargin)

    "lower to an await call" in {
      inside(cpg.call.nameExact(RustOperators.await).l) { case awaitCall :: Nil =>
        awaitCall.code shouldBe "f().await"
        awaitCall.methodFullName shouldBe RustOperators.await
        awaitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        awaitCall.typeFullName shouldBe Defines.Any
      }
    }

    "have the `f()` call as the single argument" in {
      inside(cpg.call.nameExact(RustOperators.await).argument.l) { case (future: Call) :: Nil =>
        future.name shouldBe "f"
        future.code shouldBe "f()"
        future.methodFullName shouldBe "rust2cpgtest::f"
        future.argumentIndex shouldBe 1
        // It would be a Future, but this runs without sysroot. Just recording the status quo.
        future.typeFullName shouldBe "impl "
      }
    }
  }
}

class AwaitTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "an `async fn` resolved against the sysroot" should {
    val cpg = code("async fn f() -> i32 { 1 }")

    "type the METHOD_RETURN as i32" in {
      cpg.method.name("f").methodReturn.typeFullName.l shouldBe List("i32")
    }
  }

  "an `await` expression resolved against the sysroot" should {
    val cpg = code("""
        |async fn f() -> i32 { 1 }
        |async fn g() {
        | let x = f().await;
        |}
        |""".stripMargin)

    "type the await call as i32" in {
      inside(cpg.call.nameExact(RustOperators.await).l) { case awaitCall :: Nil =>
        awaitCall.code shouldBe "f().await"
        awaitCall.typeFullName shouldBe "i32"
      }
    }

    "type the `f()` call as impl Future<Output = i32> + Sized" in {
      inside(cpg.call.nameExact(RustOperators.await).argument.l) { case (future: Call) :: Nil =>
        future.code shouldBe "f()"
        future.typeFullName shouldBe "impl core::future::future::Future<Output = i32> + core::marker::Sized"
      }
    }

    "type the LOCAL `x` as i32" in {
      cpg.local.nameExact("x").typeFullName.l shouldBe List("i32")
    }
  }
}
