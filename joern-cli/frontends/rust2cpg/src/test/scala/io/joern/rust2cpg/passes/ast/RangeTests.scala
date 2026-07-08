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
}
