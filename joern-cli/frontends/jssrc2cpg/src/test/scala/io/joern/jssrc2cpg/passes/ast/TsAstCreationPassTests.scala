package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class TsAstCreationPassTests extends AstJsSrc2CpgSuite(".ts") {

  "AST generation for simple TS constructs" should {

    "have correct structure for for-of loops" in {
      val cpg = code("""
        |for(foo().x of arr) {
        |  bar();
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "have correct structure for for-in loops" in {
      val cpg = code("""
        |for(foo().x in arr) {
        |  bar();
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "have correct structure for exported variable with array declaration" in {
      val cpg = code("""
        |module M {
        |  export var [a, b] = [1, 2];
        |}
        |""".stripMargin)
      cpg.assignment.code.l shouldBe List(
        "_tmp_1 = [1, 2]",
        "_tmp_0 = __ecma.Array.factory()",
        "a = _tmp_1[0]",
        "b = _tmp_1[1]",
        "exports.a = a",
        "exports.b = b"
      )
    }

    "have correct structure for binding pattern" in {
      val cpg = code("""
        |const a = (): string | undefined => undefined;
        |(({ [a() ?? "d"]: c = "" }) => {})();
        |""".stripMargin)
      cpg.method.name.sorted.l shouldBe List(":program", "<lambda>0", "<lambda>1")
      val params = cpg.method.nameExact("<lambda>1").parameter.l
      params.code.l shouldBe List("this", """{ [a() ?? "d"]: c = "" }""")
      params.name.l shouldBe List("this", "param1_0")
    }

    "create methods for const exports" in {
      val cpg = code("export const getApiA = (req: Request) => { const user = req.user as UserDocument; }")
      cpg.method.name.sorted.l shouldBe List(":program", "<lambda>0")
      cpg.assignment.code.l shouldBe List(
        "const user = req.user as UserDocument",
        "const getApiA = (req: Request) => { const user = req.user as UserDocument; }",
        "exports.getApiA = getApiA"
      )
      inside(cpg.method.name("<lambda>0").l) { case List(anon) =>
        anon.fullName shouldBe "Test0.ts::program:<lambda>0"
        anon.ast.isIdentifier.name.l shouldBe List("user", "req")
      }
    }

    "have correct structure for import assignments" in {
      val cpg = code("""
        |import fs = require('fs');
        |import models = require('../models/index');
        |""".stripMargin)
      cpg.assignment.code.l shouldBe List("var fs = require(\"fs\")", "var models = require(\"../models/index\")")
      cpg.local.code.l shouldBe List("fs", "models")
      val List(fsDep, modelsDep) = cpg.dependency.l
      fsDep.name shouldBe "fs"
      fsDep.dependencyGroupId shouldBe Option("fs")
      modelsDep.name shouldBe "models"
      modelsDep.dependencyGroupId shouldBe Option("../models/index")

      val List(fs, models) = cpg.imports.l
      fs.code shouldBe "import fs = require('fs')"
      fs.importedEntity shouldBe Option("fs")
      fs.importedAs shouldBe Option("fs")
      models.code shouldBe "import models = require('../models/index')"
      models.importedEntity shouldBe Option("../models/index")
      models.importedAs shouldBe Option("models")
    }

    "have correct structure for declared functions" in {
      val cpg        = code("declare function foo(arg: string): string")
      val List(func) = cpg.method("foo").l
      func.code shouldBe "declare function foo(arg: string): string"
      func.name shouldBe "foo"
      func.fullName shouldBe "Test0.ts::program:foo"
      val List(_, arg) = cpg.method("foo").parameter.l
      arg.name shouldBe "arg"
      arg.typeFullName shouldBe Defines.String
      arg.code shouldBe "arg: string"
      arg.index shouldBe 1
      val List(parentTypeDecl) = cpg.typeDecl.name(":program").l
      parentTypeDecl.bindsOut.flatMap(_.refOut).l should contain(func)
    }

    "have correct structure for type assertion" in {
      val cpg = code("let emptyArray = <VNode[]>[];")
      cpg.assignment.code.l shouldBe List("let emptyArray = <VNode[]>[]")
    }

    "have correct structure for satisfies expressions" in {
      val cpg              = code("let x = y satisfies T;")
      val List(assignment) = cpg.assignment.l
      val List(x, y)       = assignment.argument.l
      assignment.code shouldBe "let x = y satisfies T"
      x.code shouldBe "x"
      y.code shouldBe "y"
    }
  }

  private def checkForInOrOf(node: Block): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(arr)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(arr)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "arr"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(loop) = node.astChildren.isControlStructure.l
    loop.controlStructureType shouldBe ControlStructureTypes.WHILE

    val List(loopTestCall) = loop.astChildren.isCall.codeExact("!(_result_0 = _iterator_0.next()).done").l
    loopTestCall.name shouldBe Operators.not
    loopTestCall.order shouldBe 1

    val List(doneMaCall) = loopTestCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next()).done").l
    doneMaCall.name shouldBe Operators.fieldAccess

    val List(doneMaBase) = doneMaCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next())").l
    doneMaBase.name shouldBe Operators.assignment
    doneMaBase.order shouldBe 1
    doneMaBase.argumentIndex shouldBe 1

    val List(doneMaBaseLhs) = doneMaBase.astChildren.isIdentifier.order(1).l
    doneMaBaseLhs.name shouldBe "_result_0"
    doneMaBaseLhs.argumentIndex shouldBe 1

    val List(doneMaBaseRhs) = doneMaBase.astChildren.isCall.order(2).l
    doneMaBaseRhs.code shouldBe "_iterator_0.next()"
    doneMaBaseRhs.argumentIndex shouldBe 2

    val List(doneMember) = doneMaCall.astChildren.isFieldIdentifier.canonicalNameExact("done").l
    doneMember.order shouldBe 2
    doneMember.argumentIndex shouldBe 2

    val List(whileLoopBlock) = loop.astChildren.isBlock.l
    whileLoopBlock.order shouldBe 2

    val List(loopVarAssignmentCall) = whileLoopBlock.astChildren.isCall.codeExact("foo().x = _result_0.value").l
    loopVarAssignmentCall.name shouldBe Operators.assignment
    loopVarAssignmentCall.order shouldBe 1

    val fooCall = loopVarAssignmentCall.argument(1).asInstanceOf[Call]
    fooCall.code shouldBe "foo().x"
    fooCall.name shouldBe Operators.fieldAccess

    val List(barCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("bar()").l
    barCall.name shouldBe "bar"
  }

}
