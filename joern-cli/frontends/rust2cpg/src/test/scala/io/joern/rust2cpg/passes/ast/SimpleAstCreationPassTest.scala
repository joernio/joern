package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class SimpleAstCreationPassTest extends Rust2CpgSuite {

  "test 07" in {
    val cpg = code("""
        |fn main(x: i32) {
        | let y = x as i64;
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.cast).l) { case cast :: Nil =>
      cast.code shouldBe "x as i64"
      cast.methodFullName shouldBe Operators.cast
      cast.typeFullName shouldBe "i64"

      inside(cast.argument.l) { case (typeRef: TypeRef) :: (identifier: Identifier) :: Nil =>
        typeRef.code shouldBe "i64"
        typeRef.typeFullName shouldBe "i64"
        identifier.name shouldBe "x"
        identifier.code shouldBe "x"
      }
    }
  }

  "test 09" in {
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

  "test 17" in {
    val cpg = code("""
        |fn main() {
        | env_logger::init();
        |}
        |""".stripMargin)
    inside(cpg.call.name("init").l) { case init :: Nil =>
      init.argument shouldBe empty
      init.code shouldBe "env_logger::init()"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.methodFullName shouldBe "env_logger.init"
    }
  }

  "test 18" in {
    val cpg = code("""
        |fn main() {
        | a::b::c();
        |}
        |""".stripMargin)

    inside(cpg.call.name("c").l) { case c :: Nil =>
      c.code shouldBe "a::b::c()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument shouldBe empty
      c.methodFullName shouldBe "a.b.c"
    }
  }

  "test 19" in {
    val cpg = code("""
        |fn gtz(x: i32) -> bool {
        | x > 0
        |}
        |
        |fn foo() -> bool {
        | gtz(10)
        |}
        |""".stripMargin)

    inside(cpg.call.name("gtz").l) { case gtz :: Nil =>
      gtz.code shouldBe "gtz(10)"
      // TODO gtz.methodFullName shouldBe
      inside(gtz.argument.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "10"
        lit.argumentIndex shouldBe 1
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "test 20" in {
    val cpg = code("""
        |fn main() {
        | foo::<u32>();
        |}
        |""".stripMargin)

    inside(cpg.call.name("foo").l) { case foo :: Nil =>
      foo.code shouldBe "foo::<u32>()"
      // TODO
      foo.methodFullName shouldBe "foo"
      foo.argument shouldBe empty
    }
  }

  "test 21" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>, i: usize) -> i32 {
        | xs[i]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
      indexAccess.code shouldBe "xs[i]"
      indexAccess.methodFullName shouldBe Operators.indexAccess
      indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(indexAccess.argument.l) { case base :: index :: Nil =>
        base.code shouldBe "xs"
        base.argumentIndex shouldBe 1
        index.code shouldBe "i"
        index.argumentIndex shouldBe 2
      }
    }
  }

  "test 22" in {
    val cpg = code("""
        |fn foo(xs: Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
        | xs[i][j]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l.sortBy(_.code.length)) { case inner :: outer :: Nil =>
      inner.code shouldBe "xs[i]"
      outer.code shouldBe "xs[i][j]"

      inside(outer.argument.l) { case (innerArg: Call) :: (indexArg: Identifier) :: Nil =>
        innerArg shouldBe inner
        innerArg.argumentIndex shouldBe 1
        // TODO: innerArg.typeFullName shouldBe

        indexArg.code shouldBe "j"
        indexArg.argumentIndex shouldBe 2
      // TODO: indexArg.typeFullName shouldBe
      }
    }
  }

  "test 23" in {
    val cpg = code("""
        |struct Point {
        | x: i32,
        |}
        |
        |fn foo(point: Point) -> i32 {
        | point.x
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
      fieldAccess.code shouldBe "point.x"
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(fieldAccess.argument.l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
        base.code shouldBe "point"
        base.argumentIndex shouldBe 1

        field.code shouldBe "x"
        field.canonicalName shouldBe "x"
        field.argumentIndex shouldBe 2
      }
    }
  }

  "test 24" in {
    val cpg = code("""
        |fn foo(pair: (i32, i32)) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
      indexAccess.code shouldBe "pair.0"
      indexAccess.methodFullName shouldBe Operators.indexAccess
      indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(indexAccess.argument.l) { case (base: Identifier) :: (index: Literal) :: Nil =>
        base.code shouldBe "pair"
        base.argumentIndex shouldBe 1

        index.code shouldBe "0"
        index.argumentIndex shouldBe 2
        index.typeFullName shouldBe "i32"
      }
    }
  }

  "test 30" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>) {
        | xs.push(1);
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("push").l) { case push :: Nil =>
      push.code shouldBe "xs.push(1)"
      // TODO push.methodFullName shouldBe
      push.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      // TODO push.typeFullName shouldBe

      inside(push.receiver.l) { case (receiver: Identifier) :: Nil =>
        receiver.code shouldBe "xs"
        receiver.argumentIndex shouldBe 0
      // TODO receiver.typeFullName shouldBe
      }

      inside(push.argument.l) { case (receiver: Identifier) :: (lit: Literal) :: Nil =>
        receiver shouldBe push.receiver.head

        lit.code shouldBe "1"
        lit.argumentIndex shouldBe 1
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "test 31" in {
    val cpg = code("""
        |fn foo() {
        | String::from(" hello ").trim().to_string();
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("trim").l) { case trim :: Nil =>
      trim.code shouldBe """String::from(" hello ").trim()"""
      trim.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      trim.arguments(1) shouldBe empty
      // TODO trim.methodFullName shouldBe

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
      // TODO toString.methodFullName shouldBe

      inside(toString.receiver.l) { case (trim: Call) :: Nil =>
        trim.name shouldBe "trim"
        trim.code shouldBe """String::from(" hello ").trim()"""
        trim.argumentIndex shouldBe 0
      }
    }
  }

  "test 32" in {
    val cpg = code("""
        |fn main(x: i32, b: bool, p: *const i32) {
        | let a = -x;
        | let c = !b;
        | let d = *p;
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.minus).l) { case minus :: Nil =>
      minus.code shouldBe "-x"
      minus.methodFullName shouldBe Operators.minus
      minus.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(minus.argument.l) { case (x: Identifier) :: Nil =>
        x.code shouldBe "x"
        x.argumentIndex shouldBe 1
      // TODO x.typeFullName shouldBe
      }
    }

    inside(cpg.call.nameExact(Operators.logicalNot).l) { case logicalNot :: Nil =>
      logicalNot.code shouldBe "!b"
      logicalNot.methodFullName shouldBe Operators.logicalNot

      inside(logicalNot.argument.l) { case (b: Identifier) :: Nil =>
        b.code shouldBe "b"
        b.argumentIndex shouldBe 1
      // TODO b.typeFullName shouldBe
      }
    }

    inside(cpg.call.nameExact(Operators.indirection).l) { case indirection :: Nil =>
      indirection.code shouldBe "*p"
      indirection.methodFullName shouldBe Operators.indirection

      inside(indirection.argument.l) { case (p: Identifier) :: Nil =>
        p.code shouldBe "p"
        p.argumentIndex shouldBe 1
      // TODO p.typeFullName shouldBe
      }
    }
  }

}
