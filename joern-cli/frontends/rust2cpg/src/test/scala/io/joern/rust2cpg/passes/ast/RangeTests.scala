package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class RangeTests extends Rust2CpgSuite {

  "`a..b`" should {
    val cpg = code("""
        |fn main() {
        | 1..2;
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "1..2"
        block.typeFullName shouldBe "core::ops::range::Range<i32>"
        block.astChildren.size shouldBe 5
      }
    }

    "the block's first child is a LOCAL declaration" in {
      inside(cpg.block.codeExact("1..2").astChildren.order(1).l) { case (local: Local) :: Nil =>
        local.name shouldBe "tmp"
        local.typeFullName shouldBe "core::ops::range::Range<i32>"
      }
    }

    "the block's second child is an alloc assignment" in {
      inside(cpg.block.codeExact("1..2").astChildren.order(2).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe s"tmp = ${Operators.alloc}"

        inside(assign.argument(1)) { case tmp: Identifier =>
          tmp.name shouldBe "tmp"
          tmp.typeFullName shouldBe "core::ops::range::Range<i32>"
        }

        inside(assign.argument(2)) { case alloc: Call =>
          alloc.methodFullName shouldBe Operators.alloc
          alloc.argument shouldBe empty
        }
      }
    }

    "the block's third child is a field assignment (start)" in {
      inside(cpg.block.codeExact("1..2").astChildren.order(3).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.start = 1"

        inside(assign.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "tmp.start"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
          fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

          inside(fieldAccess.argument(2)) { case fieldIdent: FieldIdentifier =>
            fieldIdent.canonicalName shouldBe "start"
          }
        }

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "1"
          lit.typeFullName shouldBe "i32"
        }
      }
    }

    "the block's fourth child is a field assignment (end)" in {
      inside(cpg.block.codeExact("1..2").astChildren.order(4).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.end = 2"

        inside(assign.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "tmp.end"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess

          inside(fieldAccess.argument(2)) { case fieldIdent: FieldIdentifier =>
            fieldIdent.canonicalName shouldBe "end"
          }
        }

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "2"
          lit.typeFullName shouldBe "i32"
        }
      }
    }

    "the block's fifth child is an identifier" in {
      inside(cpg.block.codeExact("1..2").astChildren.order(5).l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "tmp"
        ident.typeFullName shouldBe "core::ops::range::Range<i32>"
      }
    }
  }

  "`a..`" should {
    val cpg = code("""
        |fn main() {
        | 1..;
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "1.."
        block.typeFullName shouldBe "core::ops::range::RangeFrom<i32>"
        block.astChildren.size shouldBe 4
      }
    }

    "the block's third child is a field assignment (start)" in {
      inside(cpg.block.codeExact("1..").astChildren.order(3).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.start = 1"

        inside(assign.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "tmp.start"

          inside(fieldAccess.argument(2)) { case fieldIdent: FieldIdentifier =>
            fieldIdent.canonicalName shouldBe "start"
          }
        }

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "1"
          lit.typeFullName shouldBe "i32"
        }
      }
    }
  }

  "`..b`" should {
    val cpg = code("""
        |fn main() {
        | ..2;
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "..2"
        block.typeFullName shouldBe "core::ops::range::RangeTo<i32>"
        block.astChildren.size shouldBe 4
      }
    }

    "the block's third child is a field assignment (end)" in {
      inside(cpg.block.codeExact("..2").astChildren.order(3).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.end = 2"

        inside(assign.argument(1)) { case fieldAccess: Call =>
          fieldAccess.code shouldBe "tmp.end"

          inside(fieldAccess.argument(2)) { case fieldIdent: FieldIdentifier =>
            fieldIdent.canonicalName shouldBe "end"
          }
        }

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "2"
          lit.typeFullName shouldBe "i32"
        }
      }
    }
  }

  "`..=b`" should {
    val cpg = code("""
        |fn main() {
        | ..=2;
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe "..=2"
        block.typeFullName shouldBe "core::ops::range::RangeToInclusive<i32>"
        block.astChildren.size shouldBe 4
      }
    }

    "the block's third child is a field assignment (end)" in {
      inside(cpg.block.codeExact("..=2").astChildren.order(3).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe "tmp.end = 2"

        inside(assign.argument(2)) { case lit: Literal =>
          lit.code shouldBe "2"
          lit.typeFullName shouldBe "i32"
        }
      }
    }
  }

  "`..`" should {
    val cpg = code("""
        |fn main() {
        | ..;
        |}
        |""".stripMargin)

    "lower into a block with appropriate type and number of children" in {
      inside(cpg.method.name("main").body.astChildren.isBlock.l) { case block :: Nil =>
        block.code shouldBe ".."
        block.typeFullName shouldBe "core::ops::range::RangeFull"
        block.astChildren.size shouldBe 3
      }
    }

    "the block's first child is a LOCAL declaration" in {
      inside(cpg.block.codeExact("..").astChildren.order(1).l) { case (local: Local) :: Nil =>
        local.name shouldBe "tmp"
        local.typeFullName shouldBe "core::ops::range::RangeFull"
      }
    }

    "the block's second child is an alloc assignment" in {
      inside(cpg.block.codeExact("..").astChildren.order(2).l) { case (assign: Call) :: Nil =>
        assign.code shouldBe s"tmp = ${Operators.alloc}"
      }
    }

    "the block's third child is an identifier" in {
      inside(cpg.block.codeExact("..").astChildren.order(3).l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "tmp"
        ident.typeFullName shouldBe "core::ops::range::RangeFull"
      }
    }
  }

  "`a..=b`" should {
    val cpg = code("""
        |fn main() {
        | let r = 1..=2;
        |}
        |""".stripMargin)

    "lower the assigned value into a static call to RangeInclusive::new" in {
      inside(cpg.assignment.argument(2).l) { case (call: Call) :: Nil =>
        call.name shouldBe "new"
        call.code shouldBe "1..=2"
        call.methodFullName shouldBe "core::ops::range::RangeInclusive<Idx>::new"
        call.typeFullName shouldBe "core::ops::range::RangeInclusive<i32>"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "the call's first argument is the start operand" in {
      inside(cpg.call.methodFullNameExact("core::ops::range::RangeInclusive<Idx>::new").argument(1).l) {
        case (lit: Literal) :: Nil =>
          lit.code shouldBe "1"
          lit.typeFullName shouldBe "i32"
      }
    }

    "the call's second argument is the end operand" in {
      inside(cpg.call.methodFullNameExact("core::ops::range::RangeInclusive<Idx>::new").argument(2).l) {
        case (lit: Literal) :: Nil =>
          lit.code shouldBe "2"
          lit.typeFullName shouldBe "i32"
      }
    }
  }
}
