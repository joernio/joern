package io.joern.abap2cpg.passes

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.testfixtures.AbapCpgFixture
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language.*

class AstCreatorTests extends AbapCpgFixture {

  "AstCreator for a standalone method" should {

    "create a method node with correct name" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").size shouldBe 1
    }

    "create accessible parameters via method.parameter" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(
            Parameter("IV_INPUT", "string"),
            Parameter("IV_FLAG", "abap_bool")
          )
        )
      )
      val method = cpg.method.nameExact("MY_METHOD").head
      method.parameter.size shouldBe 2
      method.parameter.name.l should contain allOf ("IV_INPUT", "IV_FLAG")
    }

    "have parameters with correct types" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(Parameter("IV_INPUT", "string"))
        )
      )
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.typeFullName shouldBe "string"
    }

    "have parameters with correct indices" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(
            Parameter("IV_FIRST", "string"),
            Parameter("IV_SECOND", "int")
          )
        )
      )
      val params = cpg.method.nameExact("MY_METHOD").parameter.l
      params.find(_.name == "IV_FIRST").map(_.index) shouldBe Some(1)
      params.find(_.name == "IV_SECOND").map(_.index) shouldBe Some(2)
    }

    "create a method return node" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").methodReturn.size shouldBe 1
    }

    "create a method body block" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").astChildren.isBlock.size shouldBe 1
    }

    "create a local variable accessible via method.local" in {
      val body = StatementList(
        statements = Seq(
          DataDeclaration("LV_RESULT", "string", span = noSpan)
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))

      val method = cpg.method.nameExact("MY_METHOD").head
      method.local.size shouldBe 1
      method.local.name.head shouldBe "LV_RESULT"
    }
  }

  "AstCreator for methods with body statements" should {

    "create call nodes for method calls" in {
      val body = StatementList(
        statements = Seq(
          CallExpr(targetName = "HELPER", methodName = Some("RUN"), span = noSpan)
        ),
        span = noSpan
      )
      val cpg    = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val method = cpg.method.nameExact("MY_METHOD").head
      method.ast.isCall.size should be > 0
    }

    "create assignment call nodes" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            target = IdentifierExpr("LV_X", noSpan),
            value = LiteralExpr("42", "int", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("<operator>.assignment").size shouldBe 1
    }
  }

  "AstCreator for a class with methods" should {

    "create a type declaration for the class" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.typeDecl.nameExact("MY_CLASS").size shouldBe 1
    }

    "create method with class-qualified full name" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.method.fullNameExact("MY_CLASS::MY_METHOD").size shouldBe 1
    }

    "create accessible parameters for class methods" in {
      val cpg = cpgForProgram(
        programWithClass(
          "MY_CLASS",
          "MY_METHOD",
          importing = Seq(Parameter("IV_VALUE", "int"))
        )
      )
      val method = cpg.method.fullNameExact("MY_CLASS::MY_METHOD").head
      method.parameter.size shouldBe 1
      method.parameter.name.head shouldBe "IV_VALUE"
    }
  }

  "RefEdgePass" should {

    "create REF edges from identifiers to parameters" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            target = IdentifierExpr("LV_RESULT", noSpan),
            value = IdentifierExpr("IV_INPUT", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(Parameter("IV_INPUT", "string")),
          body = Some(body)
        )
      )

      val param      = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      val identifier = cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("IV_INPUT").head
      identifier._refOut.headOption shouldBe Some(param)
    }

    "create REF edges from identifiers to locals" in {
      val body = StatementList(
        statements = Seq(
          DataDeclaration("LV_RESULT", "string", span = noSpan),
          AssignmentStmt(
            target = IdentifierExpr("LV_RESULT", noSpan),
            value = LiteralExpr("hello", "string", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))

      val local = cpg.method.nameExact("MY_METHOD").local.nameExact("LV_RESULT").head
      // Identifier "LV_RESULT" on left side of assignment should ref to local
      val identifiers = cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("LV_RESULT").l
      identifiers.exists(_._refOut.headOption.contains(local)) shouldBe true
    }
  }
}
