package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.passes.GlobalTypes.{builtinPrefix, kernelPrefix}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, Local}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment

class ArrayTests extends RubyCode2CpgFixture {

  "`[]` is represented by an alloc constructor call" in {
    val cpg = code("""
                     |[]
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.name("initialize").l

    arrayCall.methodFullName shouldBe XDefines.DynamicCallUnknownFullName
    arrayCall.code shouldBe "[]"
    arrayCall.lineNumber shouldBe Some(2)
    inside(arrayCall.argument.l) { case callBase :: Nil =>
      callBase.code shouldBe "<tmp-0>"
    }

    inside(arrayCall.astSiblings.l) { case (_: Local) :: (asgnCall: Call) :: (_: Identifier) :: Nil =>
      val asgn = asgnCall.asInstanceOf[Assignment]
      val rhs  = asgn.source.asInstanceOf[Call]
      rhs.methodFullName shouldBe Operators.alloc
      rhs.code shouldBe "[]"
    }
  }

  "`[1]` is represented by an alloc call with arguments `1`" in {
    val cpg = code("""
                     |[1]
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.name("initialize").l

    arrayCall.methodFullName shouldBe XDefines.DynamicCallUnknownFullName
    arrayCall.code shouldBe "[1]"
    arrayCall.lineNumber shouldBe Some(2)
    inside(arrayCall.argument.l) { case callBase :: Nil =>
      callBase.code shouldBe "<tmp-0>"
    }

    inside(arrayCall.parentBlock.parentBlock.lastOption) { case Some(loweringBlock: Block) =>
      val asgn = loweringBlock.astChildren.assignment.last
      val lhs  = asgn.target.asInstanceOf[Call]
      lhs.methodFullName shouldBe Operators.indexAccess
      lhs.code shouldBe "<tmp-1>[0]"

      val rhs = asgn.source.asInstanceOf[Literal]
      rhs.code shouldBe "1"
    }
  }

  "`[1,2,]` is represented by an alloc call with arguments `1`, `2`" in {
    val cpg = code("""
                     |[1,2,]
                     |""".stripMargin)

    inside(cpg.block.codeExact("[1,2,]").astChildren.assignment.where(_.source.isLiteral).l) {
      case asgn1 :: asgn2 :: Nil =>
        asgn1.code shouldBe "<tmp-1>[0] = 1"
        asgn1.source.asInstanceOf[Literal].code shouldBe "1"

        asgn2.code shouldBe "<tmp-1>[1] = 2"
        asgn2.source.asInstanceOf[Literal].code shouldBe "2"
    }
  }

  "`%w{}` is represented by an alloc call" in {
    val cpg = code("""
                     |%w{}
                     |""".stripMargin)

    val List(arrayCall) = cpg.block.codeExact("%w{}").l

    arrayCall.code shouldBe "%w{}"
    arrayCall.lineNumber shouldBe Some(2)
    arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).isEmpty shouldBe true
  }

  "`%w?foo?` is represented by an alloc call with arguments 'foo'" in {
    val cpg = code("""
                     |%w?foo?
                     |""".stripMargin)

    val List(arrayCall, _) = cpg.block.codeExact("%w?foo?").l

    arrayCall.code shouldBe "%w?foo?"
    arrayCall.lineNumber shouldBe Some(2)
    val asgns = arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l

    inside(asgns.map(_.source)) { case (foo: Literal) :: Nil =>
      foo.code shouldBe "foo"
      foo.typeFullName shouldBe s"$kernelPrefix.String"
    }
  }

  "`%i(x y)` is represented by an alloc call with arguments `:x`, `:y`" in {
    val cpg = code("""
                     |%i(x y)
                     |""".stripMargin)

    val List(arrayCall, _) = cpg.block.codeExact("%i(x y)").l

    arrayCall.code shouldBe "%i(x y)"
    arrayCall.lineNumber shouldBe Some(2)
    val asgns = arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l

    inside(asgns.map(_.source)) { case (x: Literal) :: (y: Literal) :: Nil =>
      x.code shouldBe ":x"
      x.typeFullName shouldBe s"$kernelPrefix.Symbol"

      y.code shouldBe ":y"
      y.typeFullName shouldBe s"$kernelPrefix.Symbol"
    }
  }

  "%W is represented an alloc call" in {
    val cpg = code("""%W(x#{1 + 3} y#{23} z)
        |""".stripMargin)

    val List(arrayCall, _) = cpg.block.codeExact("%W(x#{1 + 3} y#{23} z)").l

    arrayCall.code shouldBe "%W(x#{1 + 3} y#{23} z)"
    arrayCall.lineNumber shouldBe Some(1)
    val asgns = arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l

    inside(asgns.map(_.source)) { case (xFmt: Call) :: (yFmt: Call) :: (zLit: Literal) :: Nil =>
      xFmt.name shouldBe Operators.formatString
      xFmt.typeFullName shouldBe Defines.getBuiltInType(Defines.String)

      yFmt.name shouldBe Operators.formatString
      yFmt.typeFullName shouldBe Defines.getBuiltInType(Defines.String)

      val List(xFmtStr, xAddFmtStr) = xFmt.astChildren.isCall.l
      xFmtStr.name shouldBe Operators.formattedValue
      xAddFmtStr.name shouldBe Operators.formattedValue

      val List(xFmtStrAdd) = xAddFmtStr.astChildren.isCall.l
      xFmtStrAdd.name shouldBe Operators.addition

      val List(lhs, rhs) = xFmtStrAdd.argument.l
      lhs.code shouldBe "1"
      rhs.code shouldBe "3"

      val List(yFmtStr, yFmt23) = yFmt.astChildren.isCall.l
      yFmtStr.name shouldBe Operators.formattedValue
      yFmt23.name shouldBe Operators.formattedValue

      val List(yFmtStrLit: Literal) = yFmt23.argument.l: @unchecked
      yFmtStrLit.code shouldBe "23"

      zLit.code shouldBe "z"
      zLit.typeFullName shouldBe s"$kernelPrefix.String"
    }
  }

  "an implicit array constructor (Array::[]) should be lowered to an array initializer" in {
    val cpg = code("""
        |x = Array [1, 2, 3]
        |""".stripMargin)

    inside(cpg.call.nameExact("[]").l) {
      case bracketCall :: Nil =>
        bracketCall.name shouldBe "[]"
        bracketCall.methodFullName shouldBe s"$builtinPrefix.Array.[]"
        bracketCall.typeFullName shouldBe s"$builtinPrefix.Array"

        inside(bracketCall.argument.l) {
          case _ :: one :: two :: three :: Nil =>
            one.code shouldBe "1"
            two.code shouldBe "2"
            three.code shouldBe "3"
          case xs => fail(s"Expected 3 literals under the array initializer, instead got [${xs.code.mkString(", ")}]")
        }
      case xs => fail(s"Expected a single array initializer call ([]), instead got [${xs.code.mkString(", ")}]")
    }

  }

  "%I array" in {
    val cpg = code("%I(test_#{1} test_2)")

    val List(arrayCall, _) = cpg.block.codeExact("%I(test_#{1} test_2)").l

    arrayCall.code shouldBe "%I(test_#{1} test_2)"
    arrayCall.lineNumber shouldBe Some(1)
    val asgns = arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l

    inside(asgns.map(_.source)) { case (test1Fmt: Call) :: (test2: Literal) :: Nil =>
      test1Fmt.name shouldBe Operators.formatString
      test1Fmt.typeFullName shouldBe Defines.getBuiltInType(Defines.Symbol)
      test1Fmt.code shouldBe "test_#{1}"

      val List(test1FmtLit, test1FmtSymbol) = test1Fmt.astChildren.isCall.l
      test1FmtSymbol.name shouldBe Operators.formattedValue
      test1FmtSymbol.typeFullName shouldBe Defines.getBuiltInType(Defines.Symbol)
      test1FmtSymbol.code shouldBe "#{1}"

      test1FmtLit.name shouldBe Operators.formattedValue

      val List(test1FmtFinal: Literal) = test1FmtLit.argument.l: @unchecked
      test1FmtFinal.code shouldBe "test_"

      test2.code shouldBe ":test_2"
      test2.typeFullName shouldBe Defines.getBuiltInType(Defines.Symbol)
    }
  }

  "shift-left operator interpreted as a call (append)" in {
    val cpg = code("[1, 2, 3] << 4")

    inside(cpg.call("<<").headOption) {
      case Some(append) =>
        append.name shouldBe "<<"
        append.methodFullName shouldBe XDefines.DynamicCallUnknownFullName
        append.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        append.argument(0).code shouldBe "[1, 2, 3]"
        append.argument(1).code shouldBe "4"
      case None => fail(s"Expected call `<<`")
    }
  }

  "Array bodies with mixed elements" in {
    val cpg = code("[1, 2 => 1, 2 => 3]")

    val List(arrayCall, _) = cpg.block.codeExact("[1, 2 => 1, 2 => 3]").l

    arrayCall.code shouldBe "[1, 2 => 1, 2 => 3]"
    arrayCall.lineNumber shouldBe Some(1)
    val asgns = arrayCall.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l

    inside(asgns.map(_.source)) { case (argLit: Literal) :: (argAssoc: Call) :: (argAssoc2: Call) :: Nil =>
      argLit.code shouldBe "1"

      argAssoc.code shouldBe "2 => 1"
      argAssoc.methodFullName shouldBe Defines.RubyOperators.association

      argAssoc2.code shouldBe "2 => 3"
      argAssoc2.methodFullName shouldBe Defines.RubyOperators.association
    }
  }

  "Array with mixed elements" in {
    val cpg = code("""
        |[
        |   *::ApplicationSettingsHelper.visible_attributes,
        |   { default_branch_protection_defaults: [
        |     :allow_force_push,
        |     :developer_can_initial_push,
        |     {
        |       allowed_to_merge: [:access_level],
        |       allowed_to_push: [:access_level]
        |      }
        |   ] },
        |   :can_create_organization,
        |   *::ApplicationSettingsHelper.some_other_attributes,
        |]
        |""".stripMargin)

    val List(arrayCall, _) = cpg.block
      .codeExact("""[
        |   *::ApplicationSettingsHelper.visible_attributes,
        |   { d...""".stripMargin)
      .l

    arrayCall.code shouldBe
      """[
        |   *::ApplicationSettingsHelper.visible_attributes,
        |   { d...""".stripMargin
    arrayCall.lineNumber shouldBe Some(2)
    val asgns = arrayCall.astChildren
      .collect { case x: Call if x.name == Operators.assignment => x.asInstanceOf[Assignment] }
      .where(_.target.isCall.nameExact(Operators.indexAccess))
      .l

    inside(asgns.map(_.source)) {
      case (splatArgOne: Call) :: (hashLiteralArg: Block) :: (symbolArg: Literal) :: (splatArgTwo: Call) :: Nil =>
        splatArgOne.methodFullName shouldBe RubyOperators.splat
        splatArgOne.code shouldBe "*::ApplicationSettingsHelper.visible_attributes"

        symbolArg.code shouldBe ":can_create_organization"
        symbolArg.typeFullName shouldBe Defines.getBuiltInType(Defines.Symbol)

        splatArgTwo.methodFullName shouldBe RubyOperators.splat
        splatArgTwo.code shouldBe "*::ApplicationSettingsHelper.some_other_attributes"

        val List(hashInitAssignment: Call, _) =
          hashLiteralArg.astChildren.isCall.name(Operators.assignment).l: @unchecked
        val List(_: Identifier, hashInitCall: Call) = hashInitAssignment.argument.l: @unchecked
        hashInitCall.methodFullName shouldBe RubyOperators.hashInitializer
    }
  }
}
