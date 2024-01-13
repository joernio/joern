package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Operators

class CaseTests extends RubyCode2CpgFixture {
  "`case x ... end` should be represented with if-else chain and multiple match expressions should be or-ed together" in {
    val caseCode = """
      |case 0
      |  when 0 
      |    0
      |  when 1,2 then 1
      |  when 3, *[4,5] then 2
      |  when *[6] then 3
      |  else 4
      |end
      |""".stripMargin
    val cpg = code(caseCode)

    val block @ List(_) = cpg.method(":program").block.astChildren.isBlock.l

    val List(assign)   = block.astChildren.assignment.l;
    val List(lhs, rhs) = assign.argument.l

    List(lhs).isCall.name.l shouldBe List("<tmp-0>")
    List(rhs).isLiteral.code.l shouldBe List("0")

    val headIf @ List(_)           = block.astChildren.isControlStructure.l
    val ifStmts @ List(_, _, _, _) = headIf.repeat(_.astChildren.order(3).astChildren.isControlStructure)(_.emit).l;
    val conds: List[List[String]] = ifStmts.condition.map { cond =>
      val orConds = List(cond)
        .repeat(_.isCall.where(_.name(Operators.logicalOr)).argument)(
          _.emit(_.whereNot(_.isCall.name(Operators.logicalOr)))
        )
        .l
      orConds.map {
        case u: Unknown => "unknown"
        case mExpr =>
          val call @ List(_) = List(mExpr).isCall.l
          call.methodFullName.l shouldBe List("===")
          val List(lhs, rhs) = call.argument.l
          rhs.code shouldBe "<tmp-0>"
          val List(code) = List(lhs).isCall.argument(1).code.l
          code
      }.l
    }.l

    conds shouldBe List(List("0"), List("1", "2"), List("3", "unknown"), List("unknown"))
    val matchResults = ifStmts.astChildren.order(2).astChildren ++ ifStmts.last.astChildren.order(3).astChildren
    matchResults.code.l shouldBe List("0", "1", "2", "3", "4")

    // It's not ideal, but we choose the smallest containing text span that we have easily acesssible
    // as we don't have a good way to immutably update RubyNode text spans.
    ifStmts.code.l should contain only caseCode.trim
    ifStmts.condition.map(_.code.trim).l shouldBe List("0", "when 1,2 then 1", "when 3, *[4,5] then 2", "*[6]")
  }

  "`case ... end` without expression" in {
    val cpg = code("""
      |case
      |  when false, true then 0
      |  when true then 1
      |  when false, *[false,false] then 2
      |  when *[false, true] then 3
      |end
      |""".stripMargin)

    val block @ List(_) = cpg.method(":program").block.astChildren.isBlock.l

    val headIf @ List(_)           = block.astChildren.isControlStructure.l
    val ifStmts @ List(_, _, _, _) = headIf.repeat(_.astChildren.order(3).astChildren.isControlStructure)(_.emit).l;
    val conds: List[List[String]] = ifStmts.condition.map { cond =>
      val orConds = List(cond)
        .repeat(_.isCall.where(_.name(Operators.logicalOr)).argument)(
          _.emit(_.whereNot(_.isCall.name(Operators.logicalOr)))
        )
        .l
      orConds.map {
        case u: Unknown => "unknown"
        case c          => c.code
      }
    }.l
    conds shouldBe List(List("false", "true"), List("true"), List("false", "unknown"), List("unknown"))

    val matchResults = ifStmts.astChildren.order(2).astChildren.l
    matchResults.code.l shouldBe List("0", "1", "2", "3")

    ifStmts.last.astChildren.order(3).l shouldBe List()
  }
}
