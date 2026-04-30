package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTest extends Rust2CpgSuite {

  "test 01" in {
    val cpg = code("", "src/lib.rs")
    cpg.file.name.sorted.l shouldBe List("<unknown>", "src/lib.rs")
  }

  "test 02" in {
    val cpg = code("", "src/lib.rs").moreCode("", "src/main.rs")
    cpg.file.name.sorted.l shouldBe List("<unknown>", "src/lib.rs", "src/main.rs")
  }

  "test 03" in {
    val cpg = code("", "src/main.rs")
    inside(cpg.namespaceBlock.filename("src/main.rs").l) { case namespaceBlock :: Nil =>
      namespaceBlock.name shouldBe "<global>"
      namespaceBlock.fullName shouldBe "src/main.rs:<global>"
    }
  }

  "test 04" in {
    val cpg = code("", "src/main.rs")
    inside(cpg.namespaceBlock.filename("src/main.rs").astChildren.l) { case (method: Method) :: Nil =>
      method.name shouldBe "<global>"
      method.fullName shouldBe "src/main.rs:<global>"
      method.code shouldBe "<global>"
      method.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
      method.astParentFullName shouldBe "src/main.rs:<global>"

      inside(method.start.methodReturn.l) { case methodRet :: Nil =>
        methodRet.typeFullName shouldBe Defines.Any
      }

      inside(method.start.modifier.sortBy(_.order).l) { case modifier0 :: modifier1 :: Nil =>
        modifier0.modifierType shouldBe ModifierTypes.VIRTUAL
        modifier1.modifierType shouldBe ModifierTypes.MODULE
      }
    }
  }

  "test 05" in {
    val cpg = code("const MAX_SIZE: usize = 1024;")
    cpg.local.name("MAX_SIZE").typeFullName.l shouldBe List("usize")

    inside(cpg.assignment.l) { case assignment :: Nil =>
      assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignment.code shouldBe "const MAX_SIZE: usize = 1024;"
      assignment.typeFullName shouldBe Defines.Any
      assignment.methodFullName shouldBe Operators.assignment
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "MAX_SIZE"
      lhs.typeFullName shouldBe "usize"
      lhs.code shouldBe "MAX_SIZE"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "1024"
      rhs.typeFullName shouldBe "i32"
    }
  }

  "test 06" in {
    val cpg = code("""
        |fn main() {
        | const FOO: i32 = 0;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("FOO").l) { case local :: Nil =>
      local.typeFullName shouldBe "i32"
      local.code shouldBe "FOO"
    }

    inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
      assignment.code shouldBe "const FOO: i32 = 0;"
      assignment.lineNumber shouldBe Some(3)
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "FOO"
      lhs.typeFullName shouldBe "i32"
      lhs.code shouldBe "FOO"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "0"
      rhs.typeFullName shouldBe "i32"
    }
  }

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

  "test 08" in {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe Defines.Any
      local.code shouldBe "x"
    }

    inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
      assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignment.code shouldBe "let x = 1;"
      assignment.typeFullName shouldBe Defines.Any
      assignment.methodFullName shouldBe Operators.assignment
      assignment.lineNumber shouldBe Some(3)
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "x"
      // TODO
      lhs.typeFullName shouldBe Defines.Any
      lhs.code shouldBe "x"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "1"
      rhs.typeFullName shouldBe "i32"
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

  "test 10" in {
    val cpg = code("""
        |fn foo() {
        | let x: usize = 10;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe "usize"
      local.code shouldBe "x"
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "x"
      lhs.typeFullName shouldBe "usize"
      lhs.code shouldBe "x"
    }

    inside(cpg.assignment.argument(2).l) { case (lit: Literal) :: Nil =>
      lit.code shouldBe "10"
      lit.typeFullName shouldBe "i32"
    }
  }

  "test 11" in {
    val cpg = code("const TT: bool = true; const FF: bool = false;")

    cpg.literal.code("true").typeFullName.l shouldBe List("bool")
    cpg.literal.code("false").typeFullName.l shouldBe List("bool")
  }

  //

  "test 12" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x > y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l) {
      case (ifExpr: ControlStructure) :: Nil =>
        ifExpr.code shouldBe "if x > y {\n  foo();\n }"

        inside(ifExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x > y"
          condition.name shouldBe Operators.greaterThan
          condition.methodFullName shouldBe Operators.greaterThan

          inside(condition.argument.l) { case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
            lhs.name shouldBe "x"
            rhs.name shouldBe "y"
          }
        }

        inside(ifExpr.whenTrue.isBlock.l) { case (thenBlock: Block) :: Nil =>
          thenBlock.astChildren.isCall.name.l shouldBe List("foo")
        }

        ifExpr.whenFalse.l shouldBe empty
    }
  }

  "test 13" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x == y {
        |  foo();
        | } else {
        |  bar();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l) {
      case (ifExpr: ControlStructure) :: Nil =>
        inside(ifExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x == y"
          condition.name shouldBe Operators.equals
          condition.methodFullName shouldBe Operators.equals
        }

        ifExpr.whenTrue.isBlock.astChildren.isCall.name.l shouldBe List("foo")

        inside(ifExpr.whenFalse.isControlStructure.controlStructureTypeExact(ControlStructureTypes.ELSE).l) {
          case (elseExpr: ControlStructure) :: Nil =>
            elseExpr.code shouldBe "else"
            elseExpr.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("bar")
        }
    }
  }

  "test 14" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | while x < y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.WHILE).l) {
      case (whileExpr: ControlStructure) :: Nil =>
        whileExpr.code shouldBe "while x < y {\n  foo();\n }"

        inside(whileExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x < y"
          condition.name shouldBe Operators.lessThan
          condition.methodFullName shouldBe Operators.lessThan

          inside(condition.argument.l) { case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
            lhs.name shouldBe "x"
            rhs.name shouldBe "y"
          }
        }

        inside(whileExpr.astChildren.isBlock.l) { case (body: Block) :: Nil =>
          body.astChildren.isCall.name.l shouldBe List("foo")
        }
    }
  }

  "test 15" in {
    val cpg = code("""
        |fn main() {
        | loop {
        |  foo();
        |  break;
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.WHILE).l) {
      case (loopExpr: ControlStructure) :: Nil =>
        loopExpr.code shouldBe "loop {\n  foo();\n  break;\n }"

        inside(loopExpr.condition.isLiteral.l) { case (condition: Literal) :: Nil =>
          condition.code shouldBe "true"
          condition.typeFullName shouldBe "bool"
        }

        inside(loopExpr.astChildren.isBlock.l) { case (body: Block) :: Nil =>
          body.astChildren.isCall.name.l shouldBe List("foo")
          body.astChildren.isControlStructure
            .controlStructureTypeExact(ControlStructureTypes.BREAK)
            .code
            .l shouldBe List("break")
        }
    }
  }

  "test 16" in {
    val cpg = code("""
        |fn main(x: i32) {
        | while x < 10 {
        |  if x == 5 {
        |   continue;
        |  }
        |  break 1;
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.CONTINUE).l) {
      case (continueExpr: ControlStructure) :: Nil =>
        continueExpr.code shouldBe "continue"
        continueExpr.controlStructureType shouldBe ControlStructureTypes.CONTINUE
    }

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.BREAK).l) {
      case (breakExpr: ControlStructure) :: Nil =>
        breakExpr.code shouldBe "break 1"
        breakExpr.controlStructureType shouldBe ControlStructureTypes.BREAK
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

  "test 25" in {
    val cpg = code("""
        |fn main() {}
        |""".stripMargin)

    inside(cpg.method.name("main").l) { case main :: Nil =>
      main.fullName shouldBe "src/lib.rs:<global>.main"
      // TODO: main.astParentFullName shouldBe "src/lib.rs:<global>"
      main.parameter shouldBe empty
      main.methodReturn.typeFullName shouldBe "()"
      main.block.typeFullName shouldBe Defines.Any
      main.block.astChildren shouldBe empty
    }
  }

  "test 26" in {
    val cpg = code("""
        |fn main() -> i32 { 1 }
        |""".stripMargin)

    inside(cpg.method.name("main").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("main").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "1"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "1"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "test 27" in {
    val cpg = code("""
        |fn main() -> char {
        | return 'x';
        |}
        |""".stripMargin)

    cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("char")

    inside(cpg.method.name("main").block.astChildren.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "return 'x'"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "'x'"
        lit.typeFullName shouldBe "char"
      }
    }
  }

  "test 28" in {
    val cpg = code("""
        |fn id(x: i32) -> i32 {
        | x
        |}
        |""".stripMargin)

    inside(cpg.method.name("id").l) { case (method: Method) :: Nil =>
      method.fullName shouldBe "src/lib.rs:<global>.id"
    }

    inside(cpg.method.name("id").parameter.sortBy(_.order).l) { case (param: MethodParameterIn) :: Nil =>
      param.name shouldBe "x"
      param.index shouldBe 1
      param.typeFullName shouldBe "i32"
    }

    inside(cpg.method.name("id").block.astChildren.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "x"

      inside(ret.astChildren.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.code shouldBe "x"
      // TODO: ident.typeFullName shouldBe "i32"
      }
    }
  }

  "test 29" in {
    val cpg = code("""
        |fn foo(p1: i32, p2: i64, p3: f32) {}
        |""".stripMargin)

    inside(cpg.method.name("foo").l) { case (method: Method) :: Nil =>
      method.fullName shouldBe "src/lib.rs:<global>.foo"
    }

    inside(cpg.method.name("foo").parameter.sortBy(_.order).l) { case p1 :: p2 :: p3 :: Nil =>
      p1.name shouldBe "p1"
      p1.index shouldBe 1
      p1.typeFullName shouldBe "i32"

      p2.name shouldBe "p2"
      p2.index shouldBe 2
      p2.typeFullName shouldBe "i64"

      p3.name shouldBe "p3"
      p3.index shouldBe 3
      p3.typeFullName shouldBe "f32"
    }

    cpg.method.name("foo").block.astChildren shouldBe empty
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

  "test 33" in {
    val cpg = code("""
        |fn main(b: bool) {
        | if !b {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).condition.isCall.l) {
      case (condition: Call) :: Nil =>
        condition.code shouldBe "!b"
        condition.name shouldBe Operators.logicalNot
        condition.methodFullName shouldBe Operators.logicalNot
        // TODO condition.typeFullName shouldBe "bool"

        inside(condition.argument.l) { case (b: Identifier) :: Nil =>
          // TODO b.typeFullName shouldBe "bool"
          b.code shouldBe "b"
          b.name shouldBe b.code
        }
    }
  }

  "test 34" in {
    val cpg = code("""
        |fn foo() -> i32 {
        | (24)
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("foo").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "(24)"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "24"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

}
