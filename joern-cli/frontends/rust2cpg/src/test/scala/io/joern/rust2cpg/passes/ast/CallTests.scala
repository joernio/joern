package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
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

    "have an unresolved-namespace methodFullName for the inner method call" in {
      cpg.call.nameExact("chain").methodFullName.l shouldBe List(s"${Defines.UnresolvedNamespace}::chain")
    }

    "have an unresolved-namespace methodFullName for the outer method call" in {
      cpg.call.nameExact("tail").methodFullName.l shouldBe List(s"${Defines.UnresolvedNamespace}::tail")
    }

    "have an unresolved-namespace methodFullName for the function call" in {
      cpg.call.nameExact("external").methodFullName.l shouldBe List(s"${Defines.UnresolvedNamespace}::external")
    }

    "have STATIC_DISPATCH for the function call" in {
      cpg.call.nameExact("external").dispatchType.l shouldBe List(DispatchTypes.STATIC_DISPATCH)
    }

    "have STATIC_DISPATCH for each method call in the chain" in {
      cpg.call.nameExact("chain", "tail").dispatchType.toSet shouldBe Set(DispatchTypes.STATIC_DISPATCH)
    }

    "have Any as typeFullName for each call in the chain" in {
      cpg.call.nameExact("external", "chain", "tail").typeFullName.toSet shouldBe Set(Defines.Any)
    }

    "create an external method stub for each call" in {
      cpg.method.fullNameExact(s"${Defines.UnresolvedNamespace}::external").isExternal.l shouldBe List(true)
      cpg.method.fullNameExact(s"${Defines.UnresolvedNamespace}::chain").isExternal.l shouldBe List(true)
      cpg.method.fullNameExact(s"${Defines.UnresolvedNamespace}::tail").isExternal.l shouldBe List(true)
    }

    "resolve the unknown call type to an external TYPE_DECL" in {
      inside(cpg.call.nameExact("chain").typ.referencedTypeDecl.l) { case typeDecl :: Nil =>
        typeDecl.fullName shouldBe Defines.Any
        typeDecl.isExternal shouldBe true
      }
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

    "have an unresolved-namespace methodFullName" in {
      inside(cpg.call.nameExact("push").l) { case push :: Nil =>
        push.code shouldBe "xs.push(1)"
        push.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::push"
        push.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
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
        trim.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        trim.arguments(1) shouldBe empty
        trim.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::trim"

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
        toString.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        toString.arguments(1) shouldBe empty
        toString.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::to_string"

        inside(toString.receiver.l) { case (trim: Call) :: Nil =>
          trim.name shouldBe "trim"
          trim.code shouldBe """String::from(" hello ").trim()"""
          trim.argumentIndex shouldBe 0
        }
      }
    }
  }

  "a method call through a `&dyn Trait` receiver" should {
    val cpg = code("""
        |trait Greet { fn hello(&self) -> i32; }
        |fn run(g: &dyn Greet) { g.hello(); }
        |""".stripMargin)

    "have DYNAMIC_DISPATCH and a resolved methodFullName" in {
      inside(cpg.call.nameExact("hello").l) { case hello :: Nil =>
        hello.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        hello.methodFullName shouldBe "rust2cpgtest::Greet::hello"
      }
    }

    "have the adjusted trait object as receiver" in {
      inside(cpg.call.nameExact("hello").receiver.l) { case (receiver: Call) :: Nil =>
        receiver.name shouldBe Operators.addressOf
        receiver.code shouldBe "&*g"
        receiver.argumentIndex shouldBe 0
        receiver.typeFullName shouldBe "&dyn rust2cpgtest::Greet"

        inside(receiver.argument.l) { case (deref: Call) :: Nil =>
          deref.name shouldBe Operators.indirection
          deref.code shouldBe "*g"
          deref.typeFullName shouldBe "dyn rust2cpgtest::Greet"

          inside(deref.argument.l) { case (ident: Identifier) :: Nil =>
            ident.name shouldBe "g"
            ident.code shouldBe "g"
            ident.typeFullName shouldBe "&dyn rust2cpgtest::Greet"
          }
        }
      }
    }

    "have the receiver as its only argument" in {
      cpg.call.nameExact("hello").argument.l shouldBe cpg.call.nameExact("hello").receiver.l
    }
  }

  "a `<S as Tr>::m` call" should {
    val cpg = code("""
        |trait Tr { fn m(&self) -> i32; }
        |struct S;
        |impl Tr for S { fn m(&self) -> i32 { 0 } }
        |fn f(s: S) { <S as Tr>::m(&s); }
        |""".stripMargin)

    "resolve to the impl method" in {
      inside(cpg.call.codeExact("<S as Tr>::m(&s)").l) { case call :: Nil =>
        call.name shouldBe "m"
        call.methodFullName shouldBe "<rust2cpgtest::S as rust2cpgtest::Tr>::m"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        inside(call.receiver.l) { case (recv: Call) :: Nil =>
          recv.code shouldBe "&*&s"
          recv.typeFullName shouldBe "&rust2cpgtest::S"
          recv.argumentIndex shouldBe 0
        }
      }
    }
  }

  "a `Tr::m` call" should {
    val cpg = code("""
        |trait Tr { fn m(&self) -> i32; }
        |struct S;
        |impl Tr for S { fn m(&self) -> i32 { 0 } }
        |fn f(s: S) { Tr::m(&s); }
        |""".stripMargin)

    "resolve to the impl method" in {
      inside(cpg.call.codeExact("Tr::m(&s)").l) { case call :: Nil =>
        call.name shouldBe "m"
        call.methodFullName shouldBe "<rust2cpgtest::S as rust2cpgtest::Tr>::m"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        inside(call.receiver.l) { case (recv: Call) :: Nil =>
          recv.code shouldBe "&*&s"
          recv.typeFullName shouldBe "&rust2cpgtest::S"
          recv.argumentIndex shouldBe 0
        }
      }
    }
  }

  "an unresolved `<S as Clone>::clone` call" should {
    val cpg = code("""
        |struct S;
        |fn f(s: S) { <S as Clone>::clone(&s); }
        |""".stripMargin)

    "have an unresolved methodFullName" in {
      inside(cpg.call.codeExact("<S as Clone>::clone(&s)").l) { case call :: Nil =>
        call.name shouldBe "clone"
        call.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::clone"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }
  }

  "a `Tr::m` call of a `&dyn Trait`" should {
    val cpg = code("""
        |trait Tr { fn m(&self) -> i32; }
        |struct S;
        |impl Tr for S { fn m(&self) -> i32 { 0 } }
        |fn f(g: &dyn Tr) { Tr::m(g); }
        |""".stripMargin)

    "resolve to the trait method" in {
      inside(cpg.call.codeExact("Tr::m(g)").l) { case call :: Nil =>
        call.name shouldBe "m"
        call.methodFullName shouldBe "rust2cpgtest::Tr::m"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        inside(call.receiver.l) { case (recv: Call) :: Nil =>
          recv.code shouldBe "&*g"
          recv.typeFullName shouldBe "&dyn rust2cpgtest::Tr"
          recv.argumentIndex shouldBe 0
        }
      }
    }
  }

  "an inherent method called through receiver and fully-qualified syntax" should {
    val cpg = code("""
        |struct Circle {
        |    radius: f64,
        |}
        |impl Circle {
        |    fn area(&self) -> f64 {
        |        std::f64::consts::PI * self.radius * self.radius
        |    }
        |}
        |fn main() {
        |    let my_circle = Circle { radius: 5.0 };
        |    let area1 = my_circle.area();
        |    let area2 = Circle::area(&my_circle);
        |}
        |""".stripMargin)

    "have same dispatch type and methodFullName" in {
      inside(cpg.call.codeExact("my_circle.area()").l) { case methodCall :: Nil =>
        methodCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        methodCall.methodFullName shouldBe "rust2cpgtest::Circle::area"
        inside(cpg.call.codeExact("Circle::area(&my_circle)").l) { case call :: Nil =>
          call.dispatchType shouldBe methodCall.dispatchType
          call.methodFullName shouldBe methodCall.methodFullName
        }
      }
    }

    "have the adjusted receivers at argument index 0" in {
      inside(cpg.call.codeExact("my_circle.area()").receiver.l) { case (recv: Call) :: Nil =>
        recv.code shouldBe "&my_circle"
        recv.argumentIndex shouldBe 0
      }
      inside(cpg.call.codeExact("Circle::area(&my_circle)").receiver.l) { case (recv: Call) :: Nil =>
        recv.code shouldBe "&*&my_circle"
        recv.argumentIndex shouldBe 0
      }
    }
  }

  "a function passed as an argument" should {
    val cpg = code("""
        |fn handler(x: i64) -> i64 { x }
        |fn register(f: fn(i64) -> i64) {}
        |fn main() { register(handler); }
        |""".stripMargin)

    "have the adjusted MethodRef as the argument" in {
      inside(cpg.call.nameExact("register").argument.l) { case (cast: Call) :: Nil =>
        cast.name shouldBe Operators.cast
        cast.code shouldBe "handler as fn(i64) -> i64"
        cast.typeFullName shouldBe "fn(i64) -> i64"
        cast.argumentIndex shouldBe 1

        inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (ref: MethodRef) :: Nil =>
          typeRef.code shouldBe "fn(i64) -> i64"
          typeRef.typeFullName shouldBe "fn(i64) -> i64"

          ref.code shouldBe "handler"
          ref.methodFullName shouldBe "rust2cpgtest::handler"
          ref.typeFullName shouldBe "rust2cpgtest::handler"
        }
      }
    }

    "have a REF edge from the MethodRef to the method" in {
      cpg.methodRef.referencedMethod.l shouldBe cpg.method.fullNameExact("rust2cpgtest::handler").l
    }
  }

  "an associated function passed as an argument" should {
    val cpg = code("""
        |struct Counter;
        |impl Counter {
        |    fn incr(x: i64) -> i64 { x + 1 }
        |}
        |fn register(f: fn(i64) -> i64) {}
        |fn main() { register(Counter::incr); }
        |""".stripMargin)

    "have the adjusted MethodRef as the argument" in {
      inside(cpg.call.nameExact("register").argument.l) { case (cast: Call) :: Nil =>
        cast.name shouldBe Operators.cast
        cast.code shouldBe "Counter::incr as fn(i64) -> i64"
        cast.typeFullName shouldBe "fn(i64) -> i64"
        cast.argumentIndex shouldBe 1

        inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (ref: MethodRef) :: Nil =>
          typeRef.code shouldBe "fn(i64) -> i64"
          typeRef.typeFullName shouldBe "fn(i64) -> i64"

          ref.code shouldBe "Counter::incr"
          ref.methodFullName shouldBe "rust2cpgtest::Counter::incr"
          ref.typeFullName shouldBe "rust2cpgtest::Counter::incr"
        }
      }
    }

    "have a REF edge from the MethodRef to the method" in {
      cpg.methodRef.referencedMethod.l shouldBe cpg.method.fullNameExact("rust2cpgtest::Counter::incr").l
    }
  }
}

class CallTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "a call to `Vec::push` through receiver and fully-qualified syntax" should {
    val cpg = code("""
        |fn f(mut xs: Vec<i32>) {
        | xs.push(1);
        | Vec::push(&mut xs, 2);
        |}
        |""".stripMargin)

    "have same dispatch type and methodFullName" in {
      inside(cpg.call.codeExact("xs.push(1)").l) { case methodCall :: Nil =>
        methodCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        methodCall.methodFullName shouldBe "alloc::vec::Vec<T, A>::push"
        inside(cpg.call.codeExact("Vec::push(&mut xs, 2)").l) { case call :: Nil =>
          call.dispatchType shouldBe methodCall.dispatchType
          call.methodFullName shouldBe methodCall.methodFullName
        }
      }
    }

    "have the adjusted receivers at argument index 0" in {
      inside(cpg.call.codeExact("xs.push(1)").receiver.l) { case (recv: Call) :: Nil =>
        recv.code shouldBe "&xs"
        recv.argumentIndex shouldBe 0
      }
      inside(cpg.call.codeExact("Vec::push(&mut xs, 2)").receiver.l) { case (recv: Call) :: Nil =>
        recv.code shouldBe "&*&mut xs"
        recv.argumentIndex shouldBe 0
      }
    }
  }

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

    "resolve `from` to core::convert::From" in {
      inside(cpg.call.nameExact("from").l) { case from :: Nil =>
        from.methodFullName shouldBe "<alloc::string::String as core::convert::From<&str>>::from"
        from.typeFullName shouldBe "alloc::string::String"
        from.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
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

    "have the adjusted `from` result as the `trim` receiver" in {
      inside(cpg.call.nameExact("trim").receiver.l) { case (receiver: Call) :: Nil =>
        receiver.name shouldBe Operators.addressOf
        receiver.code shouldBe """&String::from(" hello ").deref()"""
        receiver.typeFullName shouldBe "&str"

        inside(receiver.argument.l) { case (deref: Call) :: Nil =>
          deref.name shouldBe "deref"
          deref.code shouldBe """String::from(" hello ").deref()"""
          deref.methodFullName shouldBe "<alloc::string::String as core::ops::deref::Deref>::deref"
          deref.typeFullName shouldBe "str"
        }
      }
    }

    "have the adjusted `trim` result as the `to_string` receiver" in {
      inside(cpg.call.nameExact("to_string").receiver.l) { case (receiver: Call) :: Nil =>
        receiver.name shouldBe Operators.addressOf
        receiver.code shouldBe """&*String::from(" hello ").trim()"""
        receiver.typeFullName shouldBe "&str"

        inside(receiver.argument.l) { case (deref: Call) :: Nil =>
          deref.name shouldBe Operators.indirection
          deref.code shouldBe """*String::from(" hello ").trim()"""
          deref.typeFullName shouldBe "str"
        }
      }
    }
  }
}
