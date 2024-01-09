package io.joern.csharpsrc2cpg.passes.ast

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.LiteralExpr
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class OperatorsTests extends CSharpCode2CpgFixture {
  "AST nodes for operators" should {
    "be created for unary operators" in {
      val cpg = code(
        basicBoilerplate("""
          |int i = 3;
          |i++;
          |i--;
          |++i;
          |--i;
          |!i;
          |~i;
          |+5;
          |-5;
          |&i;
          |""".stripMargin),
        "Program.cs"
      )

      val operatorCalls = cpg.method("Main").ast.isCall.nameNot(Operators.assignment).l
      operatorCalls.size shouldBe 9
      operatorCalls.name.l shouldBe List(
        "<operator>.postIncrement",
        "<operator>.postDecrement",
        "<operator>.preIncrement",
        "<operator>.preDecrement",
        "<operator>.logicalNot",
        "<operator>.not",
        "<operator>.plus",
        "<operator>.minus",
        "<operator>.addressOf"
      )
      operatorCalls.code.l shouldBe List("i++", "i--", "++i", "--i", "!i", "~i", "+5", "-5", "&i")
      inside(operatorCalls.nameExact(Operators.postDecrement).astChildren.l) { case List(ident: Identifier) =>
        ident.code shouldBe "i"
      }
    }
  }

  "be created for binary operators" in {
    val cpg = code(
      basicBoilerplate("""
        |int a = 3;
        |int b = 5;
        |a+b;
        |a-b;
        |a/b;
        |a%b;
        |a==b;
        |a!=b;
        |a&&b;
        |a||b;
        |a&b;
        |a|b;
        |a^b;
        |""".stripMargin),
      fileName = "Program.cs"
    )
    val operatorCalls = cpg.method("Main").ast.isCall.nameNot(Operators.assignment).l
    operatorCalls.size shouldBe 11
    operatorCalls.name.l shouldBe List(
      "<operator>.addition",
      "<operator>.subtraction",
      "<operator>.division",
      "<operator>.modulo",
      "<operator>.equals",
      "<operator>.notEquals",
      "<operator>.logicalAnd",
      "<operator>.logicalOr",
      "<operator>.and",
      "<operator>.or",
      "<operator>.xor"
    )
    operatorCalls.code.l shouldBe List("a+b", "a-b", "a/b", "a%b", "a==b", "a!=b", "a&&b", "a||b", "a&b", "a|b", "a^b")

    inside(operatorCalls.nameExact(Operators.addition).astChildren.l) { case List(lhs: Identifier, rhs: Identifier) =>
      lhs.code shouldBe "a"
      rhs.code shouldBe "b"
    }
  }

  "be created for shorthand assignment operators" in {
    val cpg = code(
      basicBoilerplate("""
          |int a = 3;
          |int b = 5;
          |a+=b;
          |a-=b;
          |a*=b;
          |a/=b;
          |a%=b;
          |a&=b;
          |a|=b;
          |a^=b;
          |a>>=b;
          |a<<=b;
          |""".stripMargin),
      fileName = "Program.cs"
    )
    val operatorCalls = cpg.method("Main").ast.isCall.nameNot(Operators.assignment).l
    operatorCalls.size shouldBe 10
    operatorCalls.name.l shouldBe List(
      "<operator>.assignmentPlus",
      "<operator>.assignmentMinus",
      "<operator>.assignmentMultiplication",
      "<operator>.assignmentDivision",
      "<operators>.assignmentModulo",
      "<operators>.assignmentAnd",
      "<operators>.assignmentOr",
      "<operators>.assignmentXor",
      "<operators>.assignmentLogicalShiftRight",
      "<operators>.assignmentShiftLeft"
    )
    operatorCalls.code.l shouldBe List("a+=b", "a-=b", "a*=b", "a/=b", "a%=b", "a&=b", "a|=b", "a^=b", "a>>=b", "a<<=b")

    inside(operatorCalls.nameExact(Operators.assignmentPlus).astChildren.l) {
      case List(lhs: Identifier, rhs: Identifier) =>
        lhs.code shouldBe "a"
        rhs.code shouldBe "b"
    }
  }

  "be created for comparison operators" in {
    val cpg = code(
      basicBoilerplate("""
          |int a = 3;
          |int b = 5;
          |a > b;
          |a < b;
          |a == b;
          |a >= b;
          |a <= b;
          |""".stripMargin),
      fileName = "Program.cs"
    )
    val operatorCalls = cpg.method("Main").ast.isCall.nameNot(Operators.assignment).l
    operatorCalls.size shouldBe 5
    operatorCalls.name.l shouldBe List(
      "<operator>.greaterThan",
      "<operator>.lessThan",
      "<operator>.equals",
      "<operator>.greaterEqualsThan",
      "<operator>.lessEqualsThan"
    )
    operatorCalls.code.l shouldBe List("a > b", "a < b", "a == b", "a >= b", "a <= b")

    inside(operatorCalls.nameExact(Operators.greaterThan).astChildren.l) {
      case List(lhs: Identifier, rhs: Identifier) =>
        lhs.code shouldBe "a"
        rhs.code shouldBe "b"
    }
  }
}
