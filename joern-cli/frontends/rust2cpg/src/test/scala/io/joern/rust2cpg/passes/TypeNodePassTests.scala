package io.joern.rust2cpg.passes

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

class TypeNodePassTests extends Rust2CpgSuite(noSysRoot = true) {

  "TypeNodePass" should {

    "create correct types for locals" in {
      val cpg = code("""
          |fn foo() {
          | let x: usize = 10;
          |}
          |""".stripMargin)
      inside(cpg.method.name("foo").block.local.name("x").l) { case local :: Nil =>
        local.evalType.l shouldBe List("usize")

        inside(local.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.name shouldBe "usize"
          typ.fullName shouldBe "usize"
          typ.isExternal shouldBe true
        }
      }
    }

    "create the ANY TYPE_DECL" in {
      val cpg = code("""
          |fn foo() {
          | let x = bar();
          |}
          |""".stripMargin)
      inside(cpg.method.name("foo").block.local.name("x").typ.referencedTypeDecl.l) { case typ :: Nil =>
        typ.name shouldBe Defines.Any
        typ.fullName shouldBe Defines.Any
        typ.isExternal shouldBe true
      }
    }

    "create correct types for integer literals" in {
      val cpg = code("""
          |fn main() {
          | let x = 42;
          |}
          |""".stripMargin)
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.evalType.l shouldBe List("i32")

        inside(lit.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "i32"
          typ.name shouldBe "i32"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for byte string literals" in {
      val cpg = code("""
          |fn main() {
          | let s = b"hi";
          |}
          |""".stripMargin)
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.evalType.l shouldBe List("&[u8; 2]")

        inside(lit.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "&[u8; 2]"
          typ.name shouldBe "&[u8; 2]"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for parameters" in {
      val cpg = code("""
          |fn id(x: i32) -> i32 {
          | x
          |}
          |""".stripMargin)
      inside(cpg.method.name("id").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("i32")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "i32"
          typ.name shouldBe "i32"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for parameters with struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn foo(y: Foo) {}
          |""".stripMargin)
      inside(cpg.method.name("foo").parameter.name("y").l) { case param :: Nil =>
        param.evalType.l shouldBe List("rust2cpgtest::Foo")
        param.typ.referencedTypeDecl.l shouldBe cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").l
      }
    }

    "create correct types for references" in {
      val cpg = code("""
          |fn f(x: &i32) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("&i32")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "&i32"
          typ.name shouldBe "&i32"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for references to struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn f(p: &Foo) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("p").l) { case param :: Nil =>
        param.evalType.l shouldBe List("&rust2cpgtest::Foo")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "&rust2cpgtest::Foo"
          typ.name shouldBe "&rust2cpgtest::Foo"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for tuples" in {
      val cpg = code("""
          |fn f(x: (i32, bool)) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("(i32, bool)")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "(i32, bool)"
          typ.name shouldBe "(i32, bool)"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for tuples of struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn f(x: (Foo, i32)) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("(rust2cpgtest::Foo, i32)")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "(rust2cpgtest::Foo, i32)"
          typ.name shouldBe "(rust2cpgtest::Foo, i32)"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for arrays" in {
      val cpg = code("""
          |fn f(x: [i32; 4]) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("[i32; 4]")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "[i32; 4]"
          typ.name shouldBe "[i32; 4]"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for arrays of struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn f(x: [Foo; 4]) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("[rust2cpgtest::Foo; 4]")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "[rust2cpgtest::Foo; 4]"
          typ.name shouldBe "[rust2cpgtest::Foo; 4]"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for slices" in {
      val cpg = code("""
          |fn f(x: &[i32]) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("&[i32]")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "&[i32]"
          typ.name shouldBe "&[i32]"
          typ.isExternal shouldBe true
        }
      }

    }

    "create correct types for slices of struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn f(x: &[Foo]) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("x").l) { case param :: Nil =>
        param.evalType.l shouldBe List("&[rust2cpgtest::Foo]")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "&[rust2cpgtest::Foo]"
          typ.name shouldBe "&[rust2cpgtest::Foo]"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for raw pointers" in {
      val cpg = code("""
          |fn f(p: *const i32) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("p").l) { case param :: Nil =>
        param.evalType.l shouldBe List("*const i32")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "*const i32"
          typ.name shouldBe "*const i32"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for raw pointers to struct type" in {
      val cpg = code("""
          |struct Foo;
          |fn f(p: *const Foo) {}
          |""".stripMargin)
      inside(cpg.method.name("f").parameter.name("p").l) { case param :: Nil =>
        param.evalType.l shouldBe List("*const rust2cpgtest::Foo")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "*const rust2cpgtest::Foo"
          typ.name shouldBe "*const rust2cpgtest::Foo"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for the never type" in {
      val cpg = code("""
          |fn f() -> ! { loop {} }
          |""".stripMargin)
      inside(cpg.method.name("f").methodReturn.l) { case ret :: Nil =>
        ret.evalType.l shouldBe List("!")

        inside(ret.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "!"
          typ.name shouldBe "!"
          typ.isExternal shouldBe true
        }
      }

    }

    "create correct types for unit returns" in {
      val cpg = code("""
          |fn f() {}
          |""".stripMargin)
      inside(cpg.method.name("f").methodReturn.l) { case ret :: Nil =>
        ret.evalType.l shouldBe List("()")

        inside(ret.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "()"
          typ.name shouldBe "()"
          typ.isExternal shouldBe true
        }
      }
    }
  }
}

class TypeNodePassTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "TypeNodePass" should {

    "create correct types for vec! macro locals" in {
      val cpg = code("""
          |fn foo() {
          | let v = vec![1, 2, 3];
          |}
          |""".stripMargin)
      inside(cpg.method.name("foo").block.local.name("v").l) { case local :: Nil =>
        local.evalType.l shouldBe List("alloc::vec::Vec<i32, alloc::alloc::Global>")

        inside(local.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "alloc::vec::Vec<i32, alloc::alloc::Global>"
          typ.name shouldBe "Vec"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for generic Vec parameters" in {
      val cpg = code("""
          |fn foo(xs: Vec<i32>) {}
          |""".stripMargin)
      inside(cpg.method.name("foo").parameter.name("xs").l) { case param :: Nil =>
        param.evalType.l shouldBe List("alloc::vec::Vec<i32>")

        inside(param.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "alloc::vec::Vec<i32>"
          typ.name shouldBe "Vec"
          typ.isExternal shouldBe true
        }
      }
    }

    "create correct types for String locals" in {
      val cpg = code("""
          |fn foo() {
          | let s = String::new();
          |}
          |""".stripMargin)
      inside(cpg.method.name("foo").block.local.name("s").l) { case local :: Nil =>
        local.evalType.l shouldBe List("alloc::string::String")

        inside(local.typ.referencedTypeDecl.l) { case typ :: Nil =>
          typ.fullName shouldBe "alloc::string::String"
          typ.name shouldBe "String"
          typ.isExternal shouldBe true
        }
      }
    }
  }
}
