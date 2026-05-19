package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class CallTests extends Rust2CpgSuite(noSysRoot = true) {

  "`let x = foo()`" should {
    val cpg = code("""
        |fn main() {
        | let x = foo();
        |}
        |""".stripMargin)

    "create a local for the binding" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe Defines.Any
        local.code shouldBe "x"
      }
    }

    "have the call as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Call) :: Nil =>
        rhs.name shouldBe "foo"
        rhs.code shouldBe "foo()"
        rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::foo"
        rhs.typeFullName shouldBe Defines.Any
      }
    }
  }

  "an unresolved fully-qualified call" should {
    val cpg = code("""
        |fn main() {
        | a::b::c();
        |}
        |""".stripMargin)

    "preserve the full path in methodFullName" in {
      inside(cpg.call.name("c").l) { case cCall :: Nil =>
        cCall.code shouldBe "a::b::c()"
        cCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        cCall.argument shouldBe empty
        cCall.methodFullName shouldBe "a::b::c"
      }
    }
  }

  "an unresolved chained method call" should {
    val cpg = code("""
        |fn main() {
        | external().chain().tail();
        |}
        |""".stripMargin)

    "have DynamicCallUnknownFullName for the inner method call" in {
      cpg.call.nameExact("chain").methodFullName.l shouldBe List(Defines.DynamicCallUnknownFullName)
    }

    "have DynamicCallUnknownFullName for the outer method call" in {
      cpg.call.nameExact("tail").methodFullName.l shouldBe List(Defines.DynamicCallUnknownFullName)
    }

    "have an unresolved-namespace methodFullName for the function call" in {
      cpg.call.nameExact("external").methodFullName.l shouldBe List(s"${Defines.UnresolvedNamespace}::external")
    }

    "have STATIC_DISPATCH for the function call" in {
      cpg.call.nameExact("external").dispatchType.l shouldBe List(DispatchTypes.STATIC_DISPATCH)
    }

    "have DYNAMIC_DISPATCH for each method call in the chain" in {
      cpg.call.nameExact("chain", "tail").dispatchType.toSet shouldBe Set(DispatchTypes.DYNAMIC_DISPATCH)
    }

    "have Any as typeFullName for each call in the chain" in {
      cpg.call.nameExact("external", "chain", "tail").typeFullName.toSet shouldBe Set(Defines.Any)
    }
  }

  "a call to a function defined in the same file" should {
    val cpg = code("""
        |fn callee() {}
        |fn main() { callee(); }
        |""".stripMargin)

    "have a crate-prefixed methodFullName" in {
      cpg.call.name("callee").methodFullName.l shouldBe List("rust2cpgtest::callee")
    }
  }

  "a `Vec::push` call" should {
    val cpg = code("""
        |fn foo(xs: Vec<i32>) {
        | xs.push(1);
        |}
        |""".stripMargin)

    "have DynamicCallUnknownFullName as methodFullName" in {
      inside(cpg.call.nameExact("push").l) { case push :: Nil =>
        push.code shouldBe "xs.push(1)"
        push.methodFullName shouldBe Defines.DynamicCallUnknownFullName
        push.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        push.typeFullName shouldBe Defines.Any
      }
    }

    "have the variable as receiver" in {
      inside(cpg.call.nameExact("push").receiver.l) { case (receiver: Identifier) :: Nil =>
        receiver.code shouldBe "xs"
        receiver.argumentIndex shouldBe 0
        receiver.typeFullName shouldBe Defines.Any
      }
    }

    "have the receiver and the literal as arguments" in {
      inside(cpg.call.nameExact("push").argument.l) { case (receiver: Identifier) :: (lit: Literal) :: Nil =>
        receiver shouldBe cpg.call.nameExact("push").receiver.head

        lit.code shouldBe "1"
        lit.argumentIndex shouldBe 1
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "a `String` method chain" should {
    val cpg = code("""
        |fn foo() {
        | String::from(" hello ").trim().to_string();
        |}
        |""".stripMargin)

    "lower `trim` as a method call on the result of `from`" in {
      inside(cpg.call.nameExact("trim").l) { case trim :: Nil =>
        trim.code shouldBe """String::from(" hello ").trim()"""
        trim.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        trim.arguments(1) shouldBe empty
        trim.methodFullName shouldBe Defines.DynamicCallUnknownFullName

        inside(trim.receiver.l) { case (from: Call) :: Nil =>
          from.name shouldBe "from"
          from.code shouldBe """String::from(" hello ")"""
          from.argumentIndex shouldBe 0
        }
      }
    }

    "lower `to_string` as a method call on the result of `trim`" in {
      inside(cpg.call.nameExact("to_string").l) { case toString :: Nil =>
        toString.code shouldBe """String::from(" hello ").trim().to_string()"""
        toString.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        toString.arguments(1) shouldBe empty
        toString.methodFullName shouldBe Defines.DynamicCallUnknownFullName

        inside(toString.receiver.l) { case (trim: Call) :: Nil =>
          trim.name shouldBe "trim"
          trim.code shouldBe """String::from(" hello ").trim()"""
          trim.argumentIndex shouldBe 0
        }
      }
    }
  }
}

class CallTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "a `Vec` method call resolved against the sysroot" should {
    val cpg = code("""
        |fn foo(xs: Vec<i32>) -> usize {
        | xs.push(1);
        | xs.len()
        |}
        |""".stripMargin)

    "resolve `push` to alloc::vec::Vec" in {
      inside(cpg.call.nameExact("push").l) { case push :: Nil =>
        push.methodFullName shouldBe "alloc::vec::Vec<T, A>::push"
        push.typeFullName shouldBe "()"
      }
    }

    "resolve `len` to alloc::vec::Vec" in {
      inside(cpg.call.nameExact("len").l) { case len :: Nil =>
        len.methodFullName shouldBe "alloc::vec::Vec<T, A>::len"
        len.typeFullName shouldBe "usize"
      }
    }
  }

  "a `String` method chain resolved against the sysroot" should {
    val cpg = code("""
        |fn foo() -> String {
        | String::from(" hello ").trim().to_string()
        |}
        |""".stripMargin)

    // TODO: we would expect the typeFullName to be `String`, not `&str`.
    //  Need to confirm if that's from rust-analyzer or rust_ast_gen.
    "resolve `from` to core::convert::From" in {
      inside(cpg.call.nameExact("from").l) { case from :: Nil =>
        from.methodFullName shouldBe "core::convert::From<T>::from"
        from.typeFullName shouldBe "&str"
      }
    }

    "resolve `trim` to str::trim" in {
      inside(cpg.call.nameExact("trim").l) { case trim :: Nil =>
        trim.code shouldBe """String::from(" hello ").trim()"""
        trim.methodFullName shouldBe "str::trim"
        trim.typeFullName shouldBe "&str"
      }
    }

    "resolve `to_string` to alloc::string::ToString" in {
      inside(cpg.call.nameExact("to_string").l) { case toString :: Nil =>
        toString.methodFullName shouldBe "<T as alloc::string::ToString>::to_string"
        toString.typeFullName shouldBe "alloc::string::String"
      }
    }
  }
}
