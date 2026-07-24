package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, NodeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class StructTests extends Rust2CpgSuite(noSysRoot = true) {

  "a top-level named-field struct" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("struct Foo { x: i32, y: i32 }")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::Foo")
    }

    "be parented by the fake global method" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case foo :: Nil =>
        foo.astParentType shouldBe NodeTypes.METHOD
        foo.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
      }
    }

    "have one MEMBER per field" in {
      inside(cpg.typeDecl.nameExact("Foo").member.l) { case x :: y :: Nil =>
        x.name shouldBe "x"
        x.code shouldBe "x: i32"
        x.typeFullName shouldBe "i32"
        y.name shouldBe "y"
        y.code shouldBe "y: i32"
        y.typeFullName shouldBe "i32"
      }
    }

    "have a constructor method" in {
      inside(cpg.typeDecl.nameExact("Foo").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Foo::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        init.methodReturn.typeFullName shouldBe "()"
      }
    }

    "have one parameter per field, plus self" in {
      inside(cpg.typeDecl.nameExact("Foo").method.parameter.sortBy(_.index).l) {
        case paramSelf :: paramX :: paramY :: Nil =>
          paramSelf.name shouldBe "self"
          paramSelf.index shouldBe 0
          paramSelf.typeFullName shouldBe "rust2cpgtest::Foo"
          paramX.name shouldBe "x"
          paramX.index shouldBe 1
          paramX.typeFullName shouldBe "i32"
          paramY.name shouldBe "y"
          paramY.index shouldBe 2
          paramY.typeFullName shouldBe "i32"
      }
    }

    "have one field assignment per parameter" in {
      inside(cpg.typeDecl.nameExact("Foo").method.body.astChildren.isCall.l) { case assignX :: assignY :: Nil =>
        assignX.code shouldBe "(*self).x = x"
        inside(assignX.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "(*self).x"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
        }
        inside(assignX.argument(2)) { case ident: Identifier =>
          ident.name shouldBe "x"
          ident.typeFullName shouldBe "i32"
        }
        assignY.code shouldBe "(*self).y = y"
      }
    }
  }

  "a top-level tuple struct" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("struct Pair(i32, bool);")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Pair").fullName.l shouldBe List("rust2cpgtest::Pair")
    }

    "be parented by the fake global method" in {
      inside(cpg.typeDecl.nameExact("Pair").l) { case pair :: Nil =>
        pair.astParentType shouldBe NodeTypes.METHOD
        pair.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
      }
    }

    "have one MEMBER per field, named by positional index" in {
      inside(cpg.typeDecl.nameExact("Pair").member.l) { case zero :: one :: Nil =>
        zero.name shouldBe "0"
        zero.code shouldBe "i32"
        zero.typeFullName shouldBe "i32"
        one.name shouldBe "1"
        one.code shouldBe "bool"
        one.typeFullName shouldBe "bool"
      }
    }

    "have a constructor method" in {
      inside(cpg.typeDecl.nameExact("Pair").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Pair::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        init.methodReturn.typeFullName shouldBe "()"
      }
    }

    "have correct constructor parameters" in {
      inside(cpg.typeDecl.nameExact("Pair").method.parameter.sortBy(_.index).l) {
        case paramSelf :: param0 :: param1 :: Nil =>
          paramSelf.name shouldBe "self"
          paramSelf.index shouldBe 0
          paramSelf.typeFullName shouldBe "rust2cpgtest::Pair"

          param0.name shouldBe "0"
          param0.index shouldBe 1
          param0.typeFullName shouldBe "i32"

          param1.name shouldBe "1"
          param1.index shouldBe 2
          param1.typeFullName shouldBe "bool"
      }
    }

    "have an assignment for each parameter" in {
      inside(cpg.typeDecl.nameExact("Pair").method.body.astChildren.isCall.l) { case assign0 :: assign1 :: Nil =>
        assign0.code shouldBe "(*self).0 = 0"
        inside(assign0.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "(*self).0"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
        }
        inside(assign0.argument(2)) { case ident: Identifier =>
          ident.name shouldBe "0"
          ident.typeFullName shouldBe "i32"
        }
        assign1.code shouldBe "(*self).1 = 1"
      }
    }

    "have a standalone ctor wrapper" in {
      inside(cpg.method.nameExact("Pair").l) { case ctor :: Nil =>
        ctor.fullName shouldBe "rust2cpgtest::Pair"
        ctor.astParentType shouldBe NodeTypes.METHOD
        ctor.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
        ctor.modifier shouldBe empty
        ctor.methodReturn.typeFullName shouldBe "rust2cpgtest::Pair"
      }
    }

    "have correct ctor wrapper parameters" in {
      inside(cpg.method.nameExact("Pair").parameter.sortBy(_.index).l) { case param0 :: param1 :: Nil =>
        param0.name shouldBe "0"
        param0.index shouldBe 1
        param0.typeFullName shouldBe "i32"

        param1.name shouldBe "1"
        param1.index shouldBe 2
        param1.typeFullName shouldBe "bool"
      }
    }

    "have correct ctor wrapper body" in {
      inside(cpg.method.nameExact("Pair").body.astChildren.isCall.l) { case allocAssign :: initCall :: Nil =>
        allocAssign.code shouldBe s"tmp = ${Operators.alloc}"
        initCall.name shouldBe "<init>"
        initCall.methodFullName shouldBe "rust2cpgtest::Pair::<init>"
        initCall.code shouldBe "Pair::<init>(&tmp, 0, 1)"

        inside(initCall.argument.sortBy(_.argumentIndex).l) {
          case (addressOf: Call) :: (arg0: Identifier) :: (arg1: Identifier) :: Nil =>
            addressOf.code shouldBe "&tmp"
            addressOf.argumentIndex shouldBe 0
            addressOf.typeFullName shouldBe "&rust2cpgtest::Pair"

            arg0.name shouldBe "0"
            arg0.typeFullName shouldBe "i32"

            arg1.name shouldBe "1"
            arg1.typeFullName shouldBe "bool"
        }
      }
    }
  }

  "a top-level tuple struct with one field" should {
    val cpg = code("struct Wrapper(i32);")

    "have a single MEMBER named `0`" in {
      inside(cpg.typeDecl.nameExact("Wrapper").member.l) { case zero :: Nil =>
        zero.name shouldBe "0"
        zero.code shouldBe "i32"
        zero.typeFullName shouldBe "i32"
      }
    }
  }

  "a top-level tuple struct with no fields" should {
    val cpg = code("struct Empty();")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Empty").fullName.l shouldBe List("rust2cpgtest::Empty")
    }

    "have no members" in {
      cpg.typeDecl.nameExact("Empty").member shouldBe empty
    }

    "have a constructor method" in {
      inside(cpg.typeDecl.nameExact("Empty").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Empty::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        inside(init.parameter.l) { case self :: Nil =>
          self.name shouldBe "self"
          self.index shouldBe 0
          self.typeFullName shouldBe "rust2cpgtest::Empty"
        }
      }
    }

    "have a standalone ctor wrapper" in {
      inside(cpg.method.nameExact("Empty").l) { case ctorFn :: Nil =>
        ctorFn.fullName shouldBe "rust2cpgtest::Empty"
        ctorFn.parameter shouldBe empty
        ctorFn.methodReturn.typeFullName shouldBe "rust2cpgtest::Empty"
      }
    }
  }

  "a top-level unit struct" should {
    val cpg = code("struct Bar;")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Bar").fullName.l shouldBe List("rust2cpgtest::Bar")
    }

    "have no members" in {
      cpg.typeDecl.nameExact("Bar").member shouldBe empty
    }

    "have a constructor method" in {
      inside(cpg.typeDecl.nameExact("Bar").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Bar::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        inside(init.parameter.l) { case self :: Nil =>
          self.name shouldBe "self"
          self.index shouldBe 0
          self.typeFullName shouldBe "rust2cpgtest::Bar"
        }
      }
    }
  }

  "a struct inside an inline module" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg = code("""
        |mod m {
        |  struct Foo { x: i32 }
        |}
        |""".stripMargin)

    "compose its fullName with the module" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::m::Foo")
    }

    "be parented by the module's namespace block" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case foo :: Nil =>
        foo.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
        foo.astParentFullName shouldBe s"$libPath:rust2cpgtest::m"
      }
    }
  }

  "a struct inside a nested inline module" should {
    val cpg = code("""
        |mod a {
        |  mod b {
        |    struct C;
        |  }
        |}
        |""".stripMargin)

    "compose its fullName with both modules" in {
      cpg.typeDecl.nameExact("C").fullName.l shouldBe List("rust2cpgtest::a::b::C")
    }
  }

  "a struct inside a function body" should {
    val cpg = code("""
        |fn outer() {
        |  struct Inner { value: i32 }
        |}
        |""".stripMargin)

    "have its enclosing method's fullName as prefix" in {
      cpg.typeDecl.nameExact("Inner").fullName.l shouldBe List("rust2cpgtest::outer::Inner")
    }

    "be parented by its enclosing method" in {
      inside(cpg.typeDecl.nameExact("Inner").l) { case inner :: Nil =>
        inner.astParentType shouldBe NodeTypes.METHOD
        inner.astParentFullName shouldBe "rust2cpgtest::outer"
      }
    }
  }

  "a named-field struct field access" should {
    val cpg = code("""
        |struct Point {
        | x: i32,
        |}
        |
        |fn foo(point: Point) -> i32 {
        | point.x
        |}
        |""".stripMargin)

    "lower to a fieldAccess call" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "point.x"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.typeFullName shouldBe "i32"
      }
    }

    "have the lhs as the first argument" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).argument(1).l) {
        case (base: Identifier) :: Nil =>
          base.code shouldBe "point"
          base.argumentIndex shouldBe 1
          base.typeFullName shouldBe "rust2cpgtest::Point"
      }
    }

    "have the field as the second argument" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).argument(2).l) {
        case (field: FieldIdentifier) :: Nil =>
          field.code shouldBe "x"
          field.canonicalName shouldBe "x"
          field.argumentIndex shouldBe 2
      }
    }
  }

  "a struct-typed member" should {
    val cpg = code("""
        |struct Foo { x: i32 }
        |struct Bar { baz: Foo }
        |""".stripMargin)

    "have correct typeFullName for `baz`" in {
      inside(cpg.typeDecl.nameExact("Bar").member.nameExact("baz").l) { case baz :: Nil =>
        baz.typeFullName shouldBe "rust2cpgtest::Foo"
      }
    }
  }

  "an internal named-field record expression" should {
    val cpg = code("""
        |struct Foo { x: i32, y: i32 }
        |fn main() {
        | Foo { x: 1, y: 2 };
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "Foo { x: 1, y: 2 }"
        block.typeFullName shouldBe "rust2cpgtest::Foo"
        block.astChildren.size shouldBe 4 // 1 (local) + 1 (.alloc) + 1 (<init> call) + 1 (ident)
      }
    }

    "the block's first child is a LOCAL declaration" in {
      inside(cpg.block.codeExact("Foo { x: 1, y: 2 }").astChildren.order(1).l) { case (local: Local) :: Nil =>
        local.name shouldBe "tmp"
        local.typeFullName shouldBe "rust2cpgtest::Foo"
      }
    }

    "the block's second child is an alloc assignment" in {
      inside(cpg.block.codeExact("Foo { x: 1, y: 2 }").astChildren.order(2).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe s"tmp = ${Operators.alloc}"

        inside(assign.argument(1)) { case tmp: Identifier =>
          tmp.typeFullName shouldBe "rust2cpgtest::Foo"
          tmp.name shouldBe "tmp"
          tmp.code shouldBe "tmp"
        }

        inside(assign.argument(2)) { case alloc: Call =>
          alloc.methodFullName shouldBe Operators.alloc
          alloc.name shouldBe Operators.alloc
          alloc.argument shouldBe empty
        }
      }
    }

    "the block's third child is a constructor call" in {
      inside(cpg.block.codeExact("Foo { x: 1, y: 2 }").astChildren.order(3).l) { case (init: Call) :: Nil =>
        init.name shouldBe "<init>"
        init.methodFullName shouldBe "rust2cpgtest::Foo::<init>"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        init.typeFullName shouldBe "()"

        inside(init.argument(0)) { case addressOf: Call =>
          addressOf.name shouldBe Operators.addressOf
          addressOf.code shouldBe "&tmp"
          addressOf.typeFullName shouldBe "&rust2cpgtest::Foo"

          inside(addressOf.argument(1)) { case tmp: Identifier =>
            tmp.name shouldBe "tmp"
            tmp.typeFullName shouldBe "rust2cpgtest::Foo"
          }
        }

        inside(init.argument(1)) { case lit: Literal =>
          lit.code shouldBe "1"
          lit.typeFullName shouldBe "i32"
          lit.argumentName shouldBe Some("x")
        }

        inside(init.argument(2)) { case lit: Literal =>
          lit.code shouldBe "2"
          lit.typeFullName shouldBe "i32"
          lit.argumentName shouldBe Some("y")
        }
      }
    }

    "the block's fourth child is an identifier" in {
      inside(cpg.block.codeExact("Foo { x: 1, y: 2 }").astChildren.order(4).l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "tmp"
        ident.typeFullName shouldBe "rust2cpgtest::Foo"
      }
    }
  }

  "an internal record expression with a shorthand field" should {
    val cpg = code("""
        |struct Foo { x: i32, y: i32 }
        |fn main(x: i32) {
        | Foo { x, y: 2 };
        |}
        |""".stripMargin)

    "source the shorthand field argument from the in-scope identifier" in {
      inside(cpg.call.nameExact("<init>").l) { case init :: Nil =>
        inside(init.argument(1)) { case ident: Identifier =>
          ident.name shouldBe "x"
          ident.argumentName shouldBe Some("x")
        }

        inside(init.argument(2)) { case lit: Literal =>
          lit.code shouldBe "2"
          lit.argumentName shouldBe Some("y")
        }
      }
    }
  }

  "an internal record expression of a unit struct" should {
    val cpg = code("""
        |struct Bar;
        |fn main() {
        | Bar {};
        |}
        |""".stripMargin)

    "lower into a constructor call with only the receiver argument" in {
      inside(cpg.call.nameExact("<init>").l) { case init :: Nil =>
        init.methodFullName shouldBe "rust2cpgtest::Bar::<init>"

        inside(init.argument.l.sortBy(_.argumentIndex)) { case (addressOf: Call) :: Nil =>
          addressOf.name shouldBe Operators.addressOf
          addressOf.code shouldBe "&tmp"
          addressOf.typeFullName shouldBe "&rust2cpgtest::Bar"
        }
      }
    }
  }

  "an internal record expression with a spread base" should {
    val cpg = code("""
        |struct Foo { x: i32, y: i32 }
        |fn main(base: Foo) {
        | Foo { x: 9, ..base };
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "Foo { x: 9, ..base }"
        block.typeFullName shouldBe "rust2cpgtest::Foo"
        block.astChildren.size shouldBe 5 // 1 (local) + 1 (.alloc) + 1 (tmp = base) + 1 (tmp.x = v) + 1 (ident)
      }
    }

    "the block's third child is a base-copy assignment" in {
      inside(cpg.block.codeExact("Foo { x: 9, ..base }").astChildren.order(3).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp = base"

        inside(assign.argument(1)) { case tmp: Identifier =>
          tmp.name shouldBe "tmp"
          tmp.typeFullName shouldBe "rust2cpgtest::Foo"
        }

        inside(assign.argument(2)) { case base: Identifier =>
          base.name shouldBe "base"
        }
      }
    }

    "the block's fourth child is a field assignment" in {
      inside(cpg.block.codeExact("Foo { x: 9, ..base }").astChildren.order(4).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.x = 9"

        inside(assign.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "tmp.x"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
        }

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "9"
          lit.typeFullName shouldBe "i32"
        }
      }
    }
  }

  "a tuple-struct positional field access" should {
    val cpg = code("""
        |struct Pair(i32, bool);
        |
        |fn foo(pair: Pair) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    "lower to a fieldAccess call" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "pair.0"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.typeFullName shouldBe "i32"
      }
    }

    "have the lhs as the first argument" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).argument(1).l) {
        case (base: Identifier) :: Nil =>
          base.code shouldBe "pair"
          base.argumentIndex shouldBe 1
          base.typeFullName shouldBe "rust2cpgtest::Pair"
      }
    }

    "have the positional index as the second argument" in {
      inside(cpg.method.nameExact("foo").call.nameExact(Operators.fieldAccess).argument(2).l) {
        case (field: FieldIdentifier) :: Nil =>
          field.code shouldBe "0"
          field.canonicalName shouldBe "0"
          field.argumentIndex shouldBe 2
      }
    }
  }
}
