package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
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

    val block @ List(_) = cpg.method.isModule.block.astChildren.isBlock.l

    val List(assign)   = block.astChildren.assignment.l;
    val List(lhs, rhs) = assign.argument.l

    List(lhs).isIdentifier.name.l shouldBe List("<tmp-0>")
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
        case mExpr: Call if mExpr.name == "include?" =>
          val List(_, lhs, rhs) = mExpr.astChildren.l
          rhs.code shouldBe "<tmp-0>"
          s"splat:${lhs.code}"
        case mExpr: Call if mExpr.name == Operators.equals =>
          val List(lhs, rhs) = mExpr.argument.l
          rhs.code shouldBe "<tmp-0>"
          s"expr:${lhs.code}"
      }.l
    }.l

    conds shouldBe List(List("expr:0"), List("expr:1", "expr:2"), List("expr:3", "splat:[4,5]"), List("splat:[6]"))
    val matchResults = ifStmts.astChildren.order(2).astChildren ++ ifStmts.last.astChildren.order(3).astChildren
    matchResults.code.l shouldBe List("0", "1", "2", "3", "4")

    // It's not ideal, but we choose the smallest containing text span that we have easily acesssible
    // as we don't have a good way to immutably update RubyNode text spans.
    ifStmts.condition.map(_.code.trim).l shouldBe List("0", "when 1,2 then 1", "when 3, *[4,5] then 2", "*[6]")
  }

  "`case ... end` without expression" in {
    val cpg = code("""
      |case
      |  when false, true then 0
      |  when true then 1
      |  when false, *[false,false] then 2
      |  when *[false,true] then 3
      |end
      |""".stripMargin)

    val block @ List(_) = cpg.method.isModule.block.astChildren.isBlock.l

    val headIf @ List(_)           = block.astChildren.isControlStructure.l
    val ifStmts @ List(_, _, _, _) = headIf.repeat(_.astChildren.order(3).astChildren.isControlStructure)(_.emit).l;
    val conds: List[List[String]] = ifStmts.condition.map { cond =>
      val orConds = List(cond)
        .repeat(_.isCall.where(_.name(Operators.logicalOr)).argument)(
          _.emit(_.whereNot(_.isCall.name(Operators.logicalOr)))
        )
        .l
      orConds.map {
        case c: Call if c.name == "any?" =>
          val List(_, lhs) = c.astChildren.l
          s"splat:${lhs.code}"
        case e: Expression =>
          s"expr:${e.code}"
      }
    }.l
    conds shouldBe List(
      List("expr:false", "expr:true"),
      List("expr:true"),
      List("expr:false", "splat:[false,false]"),
      List("splat:[false,true]")
    )

    val matchResults = ifStmts.astChildren.order(2).astChildren.l
    matchResults.code.l shouldBe List("0", "1", "2", "3")

    ifStmts.last.astChildren.order(3).l shouldBe List()
  }

  "An array pattern match statement" in {
    val cpg = code("""
        |def self.class_for(type, location)
        |        case [type, location]
        |        in [:value, :path]
        |          ItemValuePathParsingError
        |        in [:label, :path]
        |          ItemLabelPathParsingError
        |        in [:label, :invalid]
        |          ItemLabelInvalidTypeParsingError
        |        else
        |           SomeOtherError
        |        end
        |end""".stripMargin)

    val block @ List(_) = cpg.method.name("class_for").block.astChildren.isBlock.l

    val List(assign)   = block.astChildren.assignment.l;
    val List(lhs, rhs) = assign.argument.l

    lhs.start.isIdentifier.name.l shouldBe List("<tmp-0>")
    rhs.start.isCall.code.l shouldBe List("[type, location]")

    val headIf @ List(_)        = block.astChildren.isControlStructure.l
    val ifStmts @ List(_, _, _) = headIf.repeat(_.astChildren.order(3).astChildren.isControlStructure)(_.emit).l;

    val conds: List[List[String]] = ifStmts.condition.map { cond =>
      val orConds = List(cond)
        .repeat(_.isCall.where(_.name(Operators.logicalOr)).argument)(
          _.emit(_.whereNot(_.isCall.name(Operators.logicalOr)))
        )
        .l
      orConds.map {
        case mExpr: Call if mExpr.name == "include?" =>
          val List(_, lhs, rhs) = mExpr.astChildren.l
          rhs.code shouldBe "<tmp-0>"
          s"splat:${lhs.code}"
        case mExpr: Call if mExpr.name == Operators.equals =>
          val List(lhs: Call, rhs) = mExpr.argument.l: @unchecked
          rhs.code shouldBe "<tmp-0>"
          lhs.methodFullName shouldBe Operators.arrayInitializer
          s"expr:${lhs.code}"
      }.l
    }.l

    conds shouldBe List(List("expr:[:value, :path]"), List("expr:[:label, :path]"), List("expr:[:label, :invalid]"))
  }

  "An array pattern match statement with variables in the pattern" in {
    val cpg = code("""
                     |def self.class_for(type, location)
                     |        case [type, location]
                     |        in [:value, result]
                     |          puts "#{result}"
                     |        in [:label, notResult]
                     |          puts "#{notResult}"
                     |        else
                     |          puts "else"
                     |        end
                     |end""".stripMargin)

    val List(_, resultLocal, notResultLocal) = cpg.method.name("class_for").block.astChildren.isLocal.l
    resultLocal.name shouldBe "result"
    notResultLocal.name shouldBe "notResult"

    val block @ List(_) = cpg.method.name("class_for").block.astChildren.isBlock.l

    val List(assign, _, _) = block.astChildren.assignment.l;
    val List(lhs, rhs)     = assign.argument.l

    lhs.start.isIdentifier.name.l shouldBe List("<tmp-0>")
    rhs.start.isCall.code.l shouldBe List("[type, location]")

    val headIf @ List(_)     = block.astChildren.isControlStructure.l
    val ifStmts @ List(_, _) = headIf.repeat(_.astChildren.order(3).astChildren.isControlStructure)(_.emit).l;

    val conds: List[List[String]] = ifStmts.condition.map { cond =>
      val orConds = List(cond)
        .repeat(_.isCall.where(_.name(Operators.logicalOr)).argument)(
          _.emit(_.whereNot(_.isCall.name(Operators.logicalOr)))
        )
        .l
      orConds.map {
        case mExpr: Call if mExpr.name == "include?" =>
          val List(_, lhs, rhs) = mExpr.astChildren.l
          rhs.code shouldBe "<tmp-0>"
          s"splat:${lhs.code}"
        case mExpr: Call if mExpr.name == Operators.equals =>
          val List(lhs: Call, rhs) = mExpr.argument.l: @unchecked
          rhs.code shouldBe "<tmp-0>"
          lhs.methodFullName shouldBe Operators.arrayInitializer
          s"expr:${lhs.code}"
      }.l
    }.l

    conds shouldBe List(List("expr:[:value, result]"), List("expr:[:label, notResult]"))

    inside(ifStmts.whenTrue.isBlock.astChildren.isCall.name(Operators.assignment).l) {
      case resultMatchAssignment :: notResultMatchAssignment :: Nil =>
        inside(resultMatchAssignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.name shouldBe "result"

            rhs.methodFullName shouldBe RubyOperators.arrayPatternMatch
            rhs.code shouldBe s"${RubyOperators.arrayPatternMatch}(result)"
          case xs => fail(s"Expected lhs and rhs, got [${xs.code.mkString(",")}]")
        }

        inside(notResultMatchAssignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.name shouldBe "notResult"

            rhs.methodFullName shouldBe RubyOperators.arrayPatternMatch
            rhs.code shouldBe s"${RubyOperators.arrayPatternMatch}(notResult)"
          case xs => fail(s"Expected lhs and rhs, got [${xs.code.mkString(",")}]")
        }
      case _ => fail(s"Expected two true branches")
    }
  }
}
