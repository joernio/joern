// This test file has been translated from swift/test/Parse/switch.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SwitchTests extends AbstractPassTest {

  "SwitchTests" should {

    "testSwitch9" in AstFixture(" switch x {}") { cpg =>
      val List(switchStmt) = cpg.switchBlock.l
      val List(switchExpr) = switchStmt.astChildren.isIdentifier.nameExact("x").l
      switchExpr.order shouldBe 1
      switchExpr.code shouldBe "x"
      val List(switchBlock) = switchStmt.astChildren.isBlock.l
      switchBlock._jumpTargetViaAstOut shouldBe empty
    }

    "testSwitch10" in AstFixture("""
        |switch x {
        |  case 0:
        |    x = 0
        |  case 1, 2, 3:
        |    x = 1
        |  case _ where x % 2 == 0:
        |    x = 2
        |    x = 3
        |    x = 4
        |  case _ where x % 2 == 0, _ where x % 3 == 0:
        |    x = 5
        |  case 10, _ where x % 3 == 0:
        |    x = 6
        |  case _ where x % 2 == 0, 20:
        |    x = 7
        |  case var y where y % 2 == 0:
        |    x = y + 1
        |  case _ where 0:
        |    x = 8
        |  default:
        |    x = 9
        |}
        |""".stripMargin) { cpg =>
      val List(switchStmt) = cpg.switchBlock.l
      val List(switchExpr) = switchStmt.astChildren.isIdentifier.nameExact("x").l
      switchExpr.order shouldBe 1
      switchExpr.code shouldBe "x"
      val List(switchBlock) = switchStmt.astChildren.isBlock.l
      val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 0:").l
      caseLabel1.order shouldBe 1

      val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("0").l
      caseExpr1.order shouldBe 2

      val List(stmtExpr1) = switchBlock.astChildren.isCall.codeExact("x = 0").l
      stmtExpr1.order shouldBe 3

      switchBlock.astChildren.isControlStructure.order(4).codeExact("break").size shouldBe 1

      val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("case 1, 2, 3:").l
      caseLabel2.order shouldBe 5

      val List(caseExpr21) = switchBlock.astChildren.isLiteral.codeExact("1").l
      caseExpr21.order shouldBe 6
      val List(caseExpr22) = switchBlock.astChildren.isLiteral.codeExact("2").l
      caseExpr22.order shouldBe 7
      val List(caseExpr23) = switchBlock.astChildren.isLiteral.codeExact("3").l
      caseExpr23.order shouldBe 8

      val List(stmtExpr2) = switchBlock.astChildren.isCall.codeExact("x = 1").l
      stmtExpr2.order shouldBe 9

      switchBlock.astChildren.isControlStructure.order(10).codeExact("break").size shouldBe 1

      val List(when19) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).order(19).l
      when19.astChildren.code.l shouldBe List("!(x % 2 == 0)", "continue")
      when19.whenTrue.code.l shouldBe List("continue")
    }

    "testSwitch11" in AstFixture("""
        |// Multiple cases per case block
        |switch x {
        |  case 0:
        |  case 1:
        |    x = 0
        |}
        |""".stripMargin) { cpg =>
      val List(switchStmt) = cpg.switchBlock.l
      val List(switchExpr) = switchStmt.astChildren.isIdentifier.nameExact("x").l
      switchExpr.order shouldBe 1
      switchExpr.code shouldBe "x"
      val List(switchBlock) = switchStmt.astChildren.isBlock.l
      val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 0:").l
      caseLabel1.order shouldBe 1

      val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("0").l
      caseExpr1.order shouldBe 2

      switchBlock.astChildren.isControlStructure.order(3).codeExact("break").size shouldBe 1

      val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
      caseLabel2.order shouldBe 4

      val List(caseExpr21) = switchBlock.astChildren.isLiteral.codeExact("1").l
      caseExpr21.order shouldBe 5

      val List(stmtExpr2) = switchBlock.astChildren.isCall.codeExact("x = 0").l
      stmtExpr2.order shouldBe 6

      switchBlock.astChildren.isControlStructure.order(7).codeExact("break").size shouldBe 1
    }

    "testSwitch12" in AstFixture("""
        |switch x {
        |  case 0:
        |  default:
        |    x = 0
        |}
        |""".stripMargin) { cpg =>
      val List(switchStmt) = cpg.switchBlock.l
      val List(switchExpr) = switchStmt.astChildren.isIdentifier.nameExact("x").l
      switchExpr.order shouldBe 1
      switchExpr.code shouldBe "x"
      val List(switchBlock) = switchStmt.astChildren.isBlock.l
      val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 0:").l
      caseLabel1.order shouldBe 1

      val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("0").l
      caseExpr1.order shouldBe 2

      switchBlock.astChildren.isControlStructure.order(3).codeExact("break").size shouldBe 1

      val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("default:").l
      caseLabel2.order shouldBe 4

      val List(stmtExpr2) = switchBlock.astChildren.isCall.codeExact("x = 0").l
      stmtExpr2.order shouldBe 5

      switchBlock.astChildren.isControlStructure.order(6).codeExact("break").size shouldBe 1
    }

    "testSwitch28" in AstFixture("""
        |switch x {
        |  case 0:
        |    fallthrough
        |  case 1:
        |    fallthrough
        |  default:
        |    fallthrough
        |}
        |""".stripMargin) { cpg =>
      val List(switchStmt) = cpg.switchBlock.l
      val List(switchExpr) = switchStmt.astChildren.isIdentifier.nameExact("x").l
      switchExpr.order shouldBe 1
      switchExpr.code shouldBe "x"
      val List(switchBlock) = switchStmt.astChildren.isBlock.l
      val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 0:").l
      caseLabel1.order shouldBe 1

      val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("0").l
      caseExpr1.order shouldBe 2

      switchBlock.astChildren.isControlStructure
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .order(3)
        .codeExact("fallthrough")
        .size shouldBe 1

      val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
      caseLabel2.order shouldBe 4

      val List(caseExpr2) = switchBlock.astChildren.isLiteral.codeExact("1").l
      caseExpr2.order shouldBe 5

      switchBlock.astChildren.isControlStructure
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .order(6)
        .codeExact("fallthrough")
        .size shouldBe 1

      val List(caseLabel3) = switchBlock._jumpTargetViaAstOut.codeExact("default:").l
      caseLabel3.order shouldBe 7

      switchBlock.astChildren.isControlStructure
        .controlStructureType(ControlStructureTypes.CONTINUE)
        .order(8)
        .codeExact("fallthrough")
        .size shouldBe 1
    }

    "testSwitch29" ignore AstFixture("""
        |// Fallthrough can transfer control anywhere within a case and can appear
        |// multiple times in the same case.
        |switch x {
        |  case 0:
        |    if true { fallthrough }
        |    if false { fallthrough }
        |    x += 1
        |  default:
        |    x += 1
        |}""".stripMargin) { cpg => ??? }

    "testSwitch34" ignore AstFixture("""
        |// Fallthroughs can only transfer control into a case label with bindings if the previous case binds a superset of those vars.
        |switch t {
        |  case (1, 2):
        |    fallthrough
        |  case (var a, var b):
        |    t = (b, a)
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch36" ignore AstFixture("""
        |func patternVarDiffType(x: Int, y: Double) {
        |  switch (x, y) {
        |    case (1, let a):
        |  fallthrough
        |    case (let a, _):
        |  break
        |    }
        |}""".stripMargin) { cpg => ??? }

    "testSwitch38" ignore AstFixture("""
        |func test_label(x : Int) {
        |  Gronk:
        |  switch x {
        |    case 42: return
        |    }
        |  }
        |""".stripMargin) { cpg => ??? }

    "testSwitch42" ignore AstFixture("""
        |switch Whatever.Thing {
        |  case .Thing:
        |  @unknown case _:
        |    x = 0
        |}""".stripMargin) { cpg => ??? }

    "testSwitch43" ignore AstFixture("""
        |switch Whatever.Thing {
        |  case .Thing:
        |  @unknown default:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch54" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case _, _, _:
        |    break
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch55" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case let value:
        |    _ = value
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch56" ignore AstFixture("""
        |switch (Whatever.Thing, Whatever.Thing) {
        |  @unknown case (_, _):
        |    break
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch57" ignore AstFixture("""
       |switch Whatever.Thing {
       |  @unknown case is Whatever:
       |    break
       |}
       |""".stripMargin) { cpg => ??? }

    "testSwitch58" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case .Thing:
        |    break
        |}""".stripMargin) { cpg => ??? }

    "testSwitch59" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case (_): // okay
        |    break
        |}""".stripMargin) { cpg => ??? }

    "testSwitch60" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case _ where x == 0:
        |    break
        |}""".stripMargin) { cpg => ??? }

  }

}
