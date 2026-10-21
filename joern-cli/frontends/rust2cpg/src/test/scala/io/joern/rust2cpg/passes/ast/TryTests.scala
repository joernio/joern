package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.astcreation.RustOperators
import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TryTests extends Rust2CpgSuite(noSysRoot = true) {

  "a `?` expression" should {
    val cpg = code("""
        |fn g() -> Result<i32, String> { Ok(1) }
        |fn f() -> Result<i32, String> {
        |  let x = g()?;
        |  Ok(x)
        |}
        |""".stripMargin)

    "lower to a tryUnwrap call" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).l) { case tryCall :: Nil =>
        tryCall.code shouldBe "g()?"
        tryCall.methodFullName shouldBe RustOperators.tryUnwrap
        tryCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        tryCall.typeFullName shouldBe Defines.Any
      }
    }

    "have the `g()` call as the single argument" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).argument.l) { case (inner: Call) :: Nil =>
        inner.name shouldBe "g"
        inner.code shouldBe "g()"
        inner.argumentIndex shouldBe 1
        inner.typeFullName shouldBe Defines.Any
      }
    }
  }
}

class TryTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "a `?` expression resolved against the sysroot" should {
    val cpg = code("""
        |fn g() -> Result<i32, String> { Ok(1) }
        |fn f() -> Result<i32, String> {
        |  let x = g()?;
        |  Ok(x)
        |}
        |""".stripMargin)

    "type the tryUnwrap call as i32" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).l) { case tryCall :: Nil =>
        tryCall.code shouldBe "g()?"
        tryCall.typeFullName shouldBe "i32"
      }
    }

    "type the `g()` call as Result<i32, String>" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).argument.l) { case (inner: Call) :: Nil =>
        inner.code shouldBe "g()"
        inner.methodFullName shouldBe "rust2cpgtest::g"
        inner.typeFullName shouldBe "core::result::Result<i32, alloc::string::String>"
      }
    }

    "type the LOCAL `x` as i32" in {
      cpg.local.nameExact("x").typeFullName.l shouldBe List("i32")
    }
  }

  "a `?` expression on a method call" should {
    val cpg = code("""
        |struct S;
        |impl S {
        |  fn m(&self) -> Result<i32, ()> { Ok(1) }
        |}
        |fn f(s: S) -> Result<i32, ()> {
        |  let x = s.m()?;
        |  Ok(x)
        |}
        |""".stripMargin)

    "type the tryUnwrap call as i32" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).l) { case tryCall :: Nil =>
        tryCall.code shouldBe "s.m()?"
        tryCall.typeFullName shouldBe "i32"
      }
    }

    "have the `s.m()` method call as the single argument" in {
      inside(cpg.call.nameExact(RustOperators.tryUnwrap).argument.l) { case (methodCall: Call) :: Nil =>
        methodCall.name shouldBe "m"
        methodCall.code shouldBe "s.m()"
        methodCall.methodFullName shouldBe "rust2cpgtest::S::m"
        methodCall.typeFullName shouldBe "core::result::Result<i32, ()>"
      }
    }
  }

  "a `?` expression nested in an addition" should {
    val cpg = code("""
        |fn g() -> Result<i32, String> { Ok(1) }
        |fn f() -> Result<i32, String> {
        |  let x = g()? + 1;
        |  Ok(x)
        |}
        |""".stripMargin)

    "have the tryUnwrap call as the addition's first argument" in {
      inside(cpg.call.nameExact(Operators.addition).l) { case addition :: Nil =>
        addition.code shouldBe "g()? + 1"
        addition.typeFullName shouldBe "i32"
        inside(addition.argument.l) { case (tryCall: Call) :: (one: Literal) :: Nil =>
          tryCall.name shouldBe RustOperators.tryUnwrap
          tryCall.code shouldBe "g()?"
          tryCall.argumentIndex shouldBe 1
          one.code shouldBe "1"
          one.argumentIndex shouldBe 2
          inside(tryCall.argument.l) { case (inner: Call) :: Nil =>
            inner.name shouldBe "g"
            inner.code shouldBe "g()"
            inner.methodFullName shouldBe "rust2cpgtest::g"
            inner.typeFullName shouldBe "core::result::Result<i32, alloc::string::String>"
          }
        }
      }
    }
  }
}
