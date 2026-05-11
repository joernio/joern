package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class CallTests extends Rust2CpgSuite(noSysRoot = true) {

  "call on RHS of `let` is the assignment's second argument" in {
    val cpg = code("""
        |fn main() {
        | let x = foo();
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe Defines.Any
      local.code shouldBe "x"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Call) :: Nil =>
      rhs.name shouldBe "foo"
      rhs.code shouldBe "foo()"
      rhs.methodFullName shouldBe "foo"
      rhs.typeFullName shouldBe Defines.Any
    }
  }

  "qualified call's methodFullName preserves path" in {
    val cpg = code("""
        |fn main() {
        | env_logger::init();
        |}
        |""".stripMargin)
    inside(cpg.call.name("init").l) { case init :: Nil =>
      init.argument shouldBe empty
      init.code shouldBe "env_logger::init()"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.methodFullName shouldBe "env_logger::init"
    }
  }

  "nested qualified call's methodFullName preserves path" in {
    val cpg = code("""
        |fn main() {
        | a::b::c();
        |}
        |""".stripMargin)

    inside(cpg.call.name("c").l) { case c :: Nil =>
      c.code shouldBe "a::b::c()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument shouldBe empty
      c.methodFullName shouldBe "a::b::c"
    }
  }

  "resolved call's typeFullName is the callee's return type" in {
    val cpg = code("""
        |fn id(x: i32) -> i32 { x }
        |
        |fn caller() -> i32 {
        | id(7)
        |}
        |""".stripMargin)

    inside(cpg.call.name("id").l) { case call :: Nil =>
      call.code shouldBe "id(7)"
      call.methodFullName shouldBe "rust2cpgtest::id"
      call.typeFullName shouldBe "i32"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "chained unresolved method calls use DynamicCallUnknownFullName" in {
    val cpg = code("""
        |fn main() {
        | external().chain().tail();
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("chain").l) { case chain :: Nil =>
      chain.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      chain.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      chain.typeFullName shouldBe Defines.Any
    }

    inside(cpg.call.nameExact("tail").l) { case tail :: Nil =>
      tail.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      tail.typeFullName shouldBe Defines.Any
    }
  }

  "`Vec` method calls' methodFullName and typeFullName" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>) {
        | xs.push(1);
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("push").l) { case push :: Nil =>
      push.code shouldBe "xs.push(1)"
      push.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      push.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      push.typeFullName shouldBe Defines.Any

      inside(push.receiver.l) { case (receiver: Identifier) :: Nil =>
        receiver.code shouldBe "xs"
        receiver.argumentIndex shouldBe 0
        receiver.typeFullName shouldBe Defines.Any
      }

      inside(push.argument.l) { case (receiver: Identifier) :: (lit: Literal) :: Nil =>
        receiver shouldBe push.receiver.head

        lit.code shouldBe "1"
        lit.argumentIndex shouldBe 1
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "`String` method chain's methodFullName and typeFullName" in {
    val cpg = code("""
        |fn foo() {
        | String::from(" hello ").trim().to_string();
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("trim").l) { case trim :: Nil =>
      trim.code shouldBe """String::from(" hello ").trim()"""
      trim.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      trim.arguments(1) shouldBe empty
      trim.methodFullName shouldBe Defines.DynamicCallUnknownFullName

      inside(trim.receiver.l) { case (from: Call) :: Nil =>
        from.name shouldBe "from"
        from.code shouldBe """String::from(" hello ")"""
        from.argumentIndex shouldBe 0
      }
    }

    inside(cpg.call.nameExact("to_string").l) { case toString :: Nil =>
      toString.code shouldBe """String::from(" hello ").trim().to_string()"""
      toString.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
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

class CallTestsWithSysRoot extends Rust2CpgSuite(noSysRoot = false) {

  "`Vec` method calls' methodFullName and typeFullName" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>) -> usize {
        | xs.push(1);
        | xs.len()
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("push").l) { case push :: Nil =>
      push.methodFullName shouldBe "alloc::vec::Vec<T, A>::push"
      push.typeFullName shouldBe "()"
    }

    inside(cpg.call.nameExact("len").l) { case len :: Nil =>
      len.methodFullName shouldBe "alloc::vec::Vec<T, A>::len"
      len.typeFullName shouldBe "usize"
    }
  }

  "`String` method chain's methodFullName and typeFullName" in {
    val cpg = code("""
        |fn foo() -> String {
        | String::from(" hello ").trim().to_string()
        |}
        |""".stripMargin)

    // TODO: we would expect the typeFullName to be `String`, not `&str`.
    //  Need to confirm if that's from rust-analyzer or rust_ast_gen.
    inside(cpg.call.nameExact("from").l) { case from :: Nil =>
      from.methodFullName shouldBe "core::convert::From<T>::from"
      from.typeFullName shouldBe "&str"
    }

    inside(cpg.call.nameExact("trim").l) { case trim :: Nil =>
      trim.methodFullName shouldBe "str::trim"
      trim.typeFullName shouldBe "&str"
    }

    inside(cpg.call.nameExact("to_string").l) { case toString :: Nil =>
      toString.methodFullName shouldBe "<T as alloc::string::ToString>::to_string"
      toString.typeFullName shouldBe "alloc::string::String"
    }
  }
}
