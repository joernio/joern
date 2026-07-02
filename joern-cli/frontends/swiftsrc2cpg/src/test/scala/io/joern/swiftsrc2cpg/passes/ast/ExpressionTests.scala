package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ExpressionTests extends SwiftSrc2CpgSuite {

  "ExpressionTests" should {

    "testTernary" in {
      val cpg = code("a ? b : c")
      inside(cpg.method.name("<global>").ast.isCall.l) { case List(call: Call) =>
        call.code shouldBe "a ? b : c"
        call.name shouldBe Operators.conditional
        inside(call.argument.l) { case List(cond, trueCase, falseCase) =>
          cond.code shouldBe "a"
          trueCase.code shouldBe "b"
          falseCase.code shouldBe "c"
        }
        call.lineNumber shouldBe Option(1)
        call.columnNumber shouldBe Option(1)
      }
    }

    "testBinaryPlus" in {
      val cpg = code("let a = b + c")
      inside(cpg.call.nameExact(Operators.addition).l) { case List(call: Call) =>
        call.code shouldBe "b + c"
        inside(call.argument.l) { case List(b: Identifier, c: Identifier) =>
          b.name shouldBe "b"
          c.name shouldBe "c"
        }
      }
    }

    "testBinarySubstraction" in {
      val cpg = code("let a = b - c")
      inside(cpg.call.nameExact(Operators.subtraction).l) { case List(call: Call) =>
        call.code shouldBe "b - c"
        inside(call.argument.l) { case List(b: Identifier, c: Identifier) =>
          b.name shouldBe "b"
          c.name shouldBe "c"
        }
      }
    }

    "testAddressOf" in {
      val cpg = code("let d = Data(a: &b + offset, count: &c - offset)")
      inside(cpg.call.nameExact(Operators.addressOf).l) { case List(bCall: Call, cCall: Call) =>
        bCall.code shouldBe "&b"
        cCall.code shouldBe "&c"
        inside(bCall.argument.l) { case List(b: Identifier) =>
          b.name shouldBe "b"
        }
        inside(cCall.argument.l) { case List(c: Identifier) =>
          c.name shouldBe "c"
        }
      }
    }

    "testSequence1" in {
      val cpg = code("a ? b : c ? d : e")
      inside(cpg.method.name("<global>").ast.isCall.l) { case List(call: Call, nestedCall: Call) =>
        call.code shouldBe "a ? b : c ? d : e"
        call.name shouldBe Operators.conditional
        inside(call.argument.l) { case List(cond, trueCase, falseCase: Call) =>
          cond.code shouldBe "a"
          trueCase.code shouldBe "b"
          falseCase.code shouldBe "c ? d : e"
          falseCase shouldBe nestedCall
          inside(falseCase.argument.l) { case List(condArg, trueCaseArg, falseCaseArg) =>
            condArg.code shouldBe "c"
            trueCaseArg.code shouldBe "d"
            falseCaseArg.code shouldBe "e"
          }
        }
      }
    }

    "testSequence2" in {
      val cpg = code("A as? B + C -> D is E as! F ? G = 42 : H")
      cpg.call.nameExact(Operators.cast).code.l shouldBe List("A as? B", "D is E as! F")
      cpg.call.nameExact(Operators.instanceOf).code.l shouldBe List("D is E")
      cpg.call.nameExact(Operators.addition).code.l shouldBe List("A as? B + C")
      cpg.call.nameExact(Operators.assignment).code.l shouldBe List("G = 42")
      val List(condCall) = cpg.call.nameExact(Operators.conditional).l
      condCall.code shouldBe "D is E as! F ? G = 42 : H"
      condCall.argument.code.l shouldBe List("D is E as! F", "G = 42", "H")
    }

    "testClosureLiterals" in {
      val cpg = code("""
      |let f = { @MainActor (a: Int) async -> Int in print("hi") }
      |let g = { [weak self, weak weakB = b] foo in
      |  return 0
      |}
      |""".stripMargin)
      val List(lambda0) = cpg.method.nameExact("<lambda>0").l
      lambda0.fullName shouldBe "Test0.swift:<global>.<lambda>0:(a:Swift.Int)->Swift.Int"
      lambda0.parameter.name.l shouldBe List("a")
      lambda0.parameter.typeFullName.l shouldBe List("Swift.Int")
      lambda0.ast.isCall.nameExact("print").code.l shouldBe List("""print("hi")""")
      val List(lambda1) = cpg.method.nameExact("<lambda>1").l
      lambda1.fullName shouldBe "Test0.swift:<global>.<lambda>1:(ANY)->ANY"
      lambda1.parameter.name.l shouldBe List("foo")
      lambda1.ast.isReturn.code.l shouldBe List("return 0")
    }

    "testTrailingClosures" in {
      val cpg = code("""
      |var button =  View.Button[5, 4, 3] {
      |  Text("ABC")
      |}
      |compactMap { (parserDiag) in }
      |""".stripMargin)
      val List(textCall) = cpg.call.nameExact("Text").l
      textCall.code shouldBe """Text("ABC")"""
      val List(idx) = cpg.call.nameExact(Operators.indexAccess).l
      idx.argument.code.l shouldBe List("View.Button", "<empty>", "<lambda>0")
      val List(lambda0) = cpg.method.nameExact("<lambda>0").l
      lambda0.parameter.name.l shouldBe empty
      lambda0.ast.isCall.nameExact("Text").code.l shouldBe List("""Text("ABC")""")
      val List(lambda1) = cpg.method.nameExact("<lambda>1").l
      lambda1.fullName shouldBe "Test0.swift:<global>.<lambda>1:(parserDiag:ANY)->ANY"
      lambda1.parameter.name.l shouldBe List("parserDiag")
    }

    "testNestedTypeSpecialization" in {
      val cpg            = code("Swift.Array<Array<Foo>>()")
      val List(specCall) = cpg.call.nameExact("Swift.Array<Array<Foo>>").l
      specCall.code shouldBe "Swift.Array<Array<Foo>>()"
      val List(fieldAcc) = cpg.call.nameExact(Operators.fieldAccess).l
      fieldAcc.code shouldBe "Swift.Array"
      fieldAcc.argument.code.l shouldBe List("Swift", "Array")
    }

    "testObjectLiterals" in {
      val cpg = code("""
      |#colorLiteral1()
      |#colorLiteral2(red: 1.0)
      |#colorLiteral3(red: 1.0, green: 1.0)
      |#colorLiteral4(red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0)
      |#imageLiteral1()
      |#imageLiteral2(resourceName: "foo.png")
      |#imageLiteral3(resourceName: "foo/bar/baz/qux.png")
      |#imageLiteral4(resourceName: "foo/bar/baz/quux.png")
      |""".stripMargin)
      cpg.call.name.l should contain allOf (
        "colorLiteral1",
        "colorLiteral2",
        "colorLiteral3",
        "colorLiteral4",
        "imageLiteral1",
        "imageLiteral2",
        "imageLiteral3",
        "imageLiteral4"
      )
      cpg.call.nameExact("colorLiteral4").ast.isLiteral.code.l shouldBe List("1.0", "1.0", "1.0", "1.0")
      cpg.call.nameExact("imageLiteral2").argument.isLiteral.code.l shouldBe List("\"foo.png\"")
    }

    "testKeypathExpression" in {
      val cpg = code("""
      |\a.b.c
      |\ABCProtocol[100]
      |children.filter(\.type.defaultInitialization.isEmpty)
      |""".stripMargin)
      // KeyPath expressions are not modelled; they surface as Unknown nodes via notHandledYet.
      val keypathUnknowns = cpg.unknown.code(".*\\\\.*").l
      keypathUnknowns.code.l should contain allOf (
        "\\a.b.c",
        "\\ABCProtocol[100]"
      )
      // The surrounding `children.filter(...)` call still resolves.
      cpg.call.nameExact("filter").code.l shouldBe List("children.filter(\\.type.defaultInitialization.isEmpty)")
    }

    "testInitializerExpression" in {
      val cpg            = code("Lexer.Cursor(input: input, previous: 0)")
      val List(initCall) = cpg.call.nameExact("Cursor").l
      initCall.code shouldBe "Lexer.Cursor(input: input, previous: 0)"
      initCall.argument.isIdentifier.name.l shouldBe List("input")
      initCall.argument.isLiteral.code.l shouldBe List("0")
    }

    "testCollectionLiterals" in {
      val cpg = code("""
      |[
      |  condition ? firstOption : secondOption,
      |  bar(),
      |]
      |#fancyMacro<Arg1, Arg2>(hello: "me")
      |""".stripMargin)
      val arrInits = cpg.call.nameExact(Operators.arrayInitializer).l
      arrInits.code.l should contain("[\n  condition ? firstOption : secondOption,\n  bar(),\n]")
      val List(macroCall) = cpg.call.nameExact("fancyMacro").l
      macroCall.code shouldBe """#fancyMacro<Arg1, Arg2>(hello: "me")"""
      macroCall.argument.isLiteral.code.l shouldBe List("\"me\"")
    }

    "testInterpolatedStringLiterals" in {
      val cpg = code("""
      |func foo() -> String {
      |  return "Fixit: \(range.debugDescription)"
      |}
      |""".stripMargin)
      val List(formatCall) = cpg.call.nameExact(Operators.formatString).l
      formatCall.code shouldBe "Fixit: \\(range.debugDescription)"
      formatCall.argument.isLiteral.code.l shouldBe List("\"Fixit: \"", "\"\"")
      val List(fieldAcc) = formatCall.argument.isCall.nameExact(Operators.fieldAccess).l
      fieldAcc.code shouldBe "range.debugDescription"
    }

    "testStringLiterals1" in {
      val cpg              = code(""""\(x)"""")
      val List(formatCall) = cpg.call.nameExact(Operators.formatString).l
      formatCall.code shouldBe "\\(x)"
      formatCall.argument.isIdentifier.name.l shouldBe List("x")
    }

    "testStringLiterals2" in {
      val cpg              = code(""""Foo '\(x)' bar"""")
      val List(formatCall) = cpg.call.nameExact(Operators.formatString).l
      val List(arg1, arg3) = formatCall.argument.isLiteral.l
      arg1.code shouldBe """"Foo '""""
      arg1.argumentIndex shouldBe 1
      arg3.code shouldBe """"' bar""""
      arg3.argumentIndex shouldBe 3
      val List(arg2) = formatCall.argument.isIdentifier.l
      arg2.name shouldBe "x"
      arg2.argumentIndex shouldBe 2
    }

    "testMoveExpression" in {
      val cpg = code("""
      |func foo(msg: String) {
      |  _move msg
      |  use(_move msg)
      |  let b = (_move self).buffer
      |}
      |""".stripMargin)
      // _move is parsed as a SequenceExpr; the contextual `_move` keyword surfaces as an identifier
      // alongside the operand and the trailing `.buffer` field access dangles off it.
      val List(useCall) = cpg.call.nameExact("use").l
      useCall.argument.isIdentifier.name.l shouldBe List("self", "_move")
      cpg.call.nameExact(Operators.fieldAccess).code.l should contain("_move.buffer")
    }

    "testBorrowExpression" in {
      val cpg = code("""
      |func foo(msg: String) {
      |  _borrow msg
      |  use(_borrow msg)
      |  let b = (_borrow self).buffer
      |}
      |""".stripMargin)
      // _borrow is parsed as a SequenceExpr; the contextual `_borrow` keyword surfaces as an identifier
      // alongside the operand and the trailing `.buffer` field access dangles off it.
      val List(useCall) = cpg.call.nameExact("use").l
      useCall.argument.isIdentifier.name.l shouldBe List("self", "_borrow")
      cpg.call.nameExact(Operators.fieldAccess).code.l should contain("_borrow.buffer")
    }

    "testKeywordApplyExpression" in {
      val cpg = code("""
      |optional(x: .some(23))
      |optional(x: .none)
      |var pair : (Int, Double) = makePair(a: 1, b: 2.5)
      |""".stripMargin)
      cpg.call.nameExact("optional").code.l shouldBe List("optional(x: .some(23))", "optional(x: .none)")
      val List(makePair) = cpg.call.nameExact("makePair").l
      makePair.code shouldBe "makePair(a: 1, b: 2.5)"
      makePair.argument.isLiteral.code.l shouldBe List("1", "2.5")
      val List(pairLocal) = cpg.method.nameExact("<global>").filename("Test0.swift").block.local.nameExact("pair").l
      pairLocal.typeFullName shouldBe "(Int, Double)"
    }

    "testMacroExpansionExpression" in {
      val cpg = code("""
      |#file == $0.path
      |let a = #embed("filename.txt")
      |#Test {
      |  print("This is a test")
      |}
      |""".stripMargin)
      cpg.call.nameExact("file").code.l shouldBe List("#file")
      val List(embedCall) = cpg.call.nameExact("embed").l
      embedCall.code shouldBe """#embed("filename.txt")"""
      embedCall.argument.isLiteral.code.l shouldBe List("\"filename.txt\"")
      val List(testCall) = cpg.call.nameExact("Test").l
      testCall.code should startWith("#Test {")
      cpg.method.nameExact("<lambda>0").ast.isCall.nameExact("print").code.l shouldBe List(
        """print("This is a test")"""
      )
    }

    "testIfExprInCoercion" in {
      val cpg = code("""
      |func foo() {
      |  if .random() { 0 } else { 1 } as Int
      |}
      |""".stripMargin)
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code shouldBe "if .random() { 0 } else { 1 } as Int"
      castCall.argument.isControlStructure.controlStructureType.l shouldBe List(ControlStructureTypes.IF)
    }

    "testSwitchExprInCoercion" in {
      val cpg            = code("switch Bool.random() { case true: 0 case false: 1 } as Int")
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code should endWith("as Int")
      castCall.argument.isControlStructure.controlStructureType.l shouldBe List(ControlStructureTypes.SWITCH)
    }

    "testIfExprInReturn" in {
      val cpg = code("""
        |func foo() {
        |  return if .random() { 0 } else { 1 }
        |}
        |""".stripMargin)
      inside(cpg.method.name("foo").ast.isReturn.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("if .random() {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.IF
          inside(controlStruct.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "self.random()"
          }
          controlStruct.whenTrue.code.l shouldBe List("0")
          controlStruct.whenFalse.code.l shouldBe List("1")
          controlStruct.lineNumber shouldBe Option(3)
          controlStruct.columnNumber shouldBe Option(10)
      }
    }

    "testSwitchExprInReturn" in {
      val cpg = code("""
      |func foo() {
      |  return switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      val List(retNode)      = cpg.method.nameExact("foo").ast.isReturn.l
      val List(switchStruct) = retNode.astChildren.isControlStructure.l
      switchStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      switchStruct.code should startWith("switch Bool.random()")
      switchStruct.ast.collectAll[JumpTarget].code.l shouldBe List("case true:", "case false:")
    }

    "testTryIf1" in {
      val cpg = code("""
      |func foo() -> Int {
      |  try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      // `try` unwraps to the inner if-expression — no wrapping call is created.
      val List(ifStruct) = cpg.method.nameExact("foo").ast.isControlStructure.l
      ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
      ifStruct.whenTrue.code.l shouldBe List("0")
      ifStruct.whenFalse.code.l shouldBe List("1")
    }

    "testTryIf2" in {
      val cpg = code("""
      |func foo() -> Int {
      |  return try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      val List(retNode)  = cpg.method.nameExact("foo").ast.isReturn.l
      val List(ifStruct) = retNode.astChildren.isControlStructure.l
      ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
    }

    "testTryIf3" in {
      val cpg = code("""
      |func foo() -> Int {
      |  let x = try if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin)
      val List(localX) = cpg.method.nameExact("foo").block.local.nameExact("x").l
      localX.code shouldBe "x"
      val List(ifStruct) = cpg.method.nameExact("foo").ast.isControlStructure.l
      ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
    }

    "testAwaitIf1" in {
      val cpg = code("""
      |func foo() async -> Int {
      |  await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      // `await` wraps its operand in an `<operator>.await` call.
      val List(awaitCall) = cpg.call.nameExact("<operator>.await").l
      awaitCall.code should startWith("await if .random()")
      val List(ifStruct) = cpg.method.nameExact("foo").ast.isControlStructure.l
      ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
    }

    "testAwaitIf2" in {
      val cpg = code("""
      |func foo() async -> Int {
      |  return await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      val List(retNode)   = cpg.method.nameExact("foo").ast.isReturn.l
      val List(awaitCall) = retNode.astChildren.isCall.nameExact("<operator>.await").l
      awaitCall.code should startWith("await if .random()")
    }

    "testAwaitIf3" in {
      val cpg = code("""
      |func foo() async -> Int {
      |  let x = await if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin)
      val List(localX) = cpg.method.nameExact("foo").block.local.nameExact("x").l
      localX.code shouldBe "x"
      val List(awaitCall) = cpg.call.nameExact("<operator>.await").l
      awaitCall.code should startWith("await if .random()")
    }

    "testTrySwitch1" in {
      val cpg = code("try switch Bool.random() { case true: 0 case false: 1 }")
      // `try` unwraps to the inner switch — no wrapping call.
      val List(switchStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch Bool.random()")
      switchStruct.ast.collectAll[JumpTarget].code.l shouldBe List("case true:", "case false:")
    }

    "testTrySwitch2" in {
      val cpg = code("""
      |func foo() -> Int {
      |  return try switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      val List(retNode)      = cpg.method.nameExact("foo").ast.isReturn.l
      val List(switchStruct) = retNode.astChildren.isControlStructure.l
      switchStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
    }

    "testTrySwitch3" in {
      val cpg = code("""
      |func foo() -> Int {
      |  let x = try switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin)
      val List(localX) = cpg.method.nameExact("foo").block.local.nameExact("x").l
      localX.code shouldBe "x"
      val List(switchStruct) = cpg.method
        .nameExact("foo")
        .ast
        .isControlStructure
        .controlStructureType(ControlStructureTypes.SWITCH)
        .l
      switchStruct.code should startWith("switch Bool.random()")
    }

    "testAwaitSwitch1" in {
      val cpg             = code("await switch Bool.random() { case true: 0 case false: 1 }")
      val List(awaitCall) = cpg.call.nameExact("<operator>.await").l
      awaitCall.code should startWith("await switch Bool.random()")
      val List(switchStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch Bool.random()")
    }

    "testAwaitSwitch2" in {
      val cpg = code("""
      |func foo() async -> Int {
      |  return await switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      val List(retNode)   = cpg.method.nameExact("foo").ast.isReturn.l
      val List(awaitCall) = retNode.astChildren.isCall.nameExact("<operator>.await").l
      awaitCall.code should startWith("await switch Bool.random()")
    }

    "testAwaitSwitch3" in {
      val cpg = code("""
      |func foo() async -> Int {
      |  let x = await switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin)
      val List(localX) = cpg.method.nameExact("foo").block.local.nameExact("x").l
      localX.code shouldBe "x"
      val List(awaitCall) = cpg.call.nameExact("<operator>.await").l
      awaitCall.code should startWith("await switch Bool.random()")
    }

    "testIfExprCondCast" in {
      val cpg            = code("if .random() { 0 } else { 1 } as? Int")
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code shouldBe "if .random() { 0 } else { 1 } as? Int"
      val List(ifStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifStruct.code should startWith("if .random()")
    }

    "testIfExprForceCast" in {
      val cpg            = code("if .random() { 0 } else { 1 } as! Int")
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code shouldBe "if .random() { 0 } else { 1 } as! Int"
      val List(ifStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifStruct.code should startWith("if .random()")
    }

    "testSwitchExprCondCast" in {
      val cpg            = code("switch Bool.random() { case true: 0 case false: 1 } as? Int")
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code shouldBe "switch Bool.random() { case true: 0 case false: 1 } as? Int"
      val List(switchStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch Bool.random()")
    }

    "testSwitchExprForceCast" in {
      val cpg            = code("switch Bool.random() { case true: 0 case false: 1 } as! Int")
      val List(castCall) = cpg.call.nameExact(Operators.cast).l
      castCall.code shouldBe "switch Bool.random() { case true: 0 case false: 1 } as! Int"
      val List(switchStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch Bool.random()")
    }

    "testInitCallInPoundIf" in {
      val cpg = code("""
      |class C {
      |  init() {
      |  #if true
      |    init()
      |  #endif
      |  }
      |}
      |""".stripMargin)
      val List(initMethod) = cpg.typeDecl.nameExact("C").method.nameExact("init").l
      initMethod.fullName shouldBe "Test0.swift:<global>.C.init:()->Test0.swift:<global>.C"
      initMethod.signature shouldBe "()->Test0.swift:<global>.C"
      initMethod.parameter.name.l shouldBe List("self")
    }

    "testClosureParameterWithModifier" in {
      val cpg                 = code("_ = { (_const x: Int) in }")
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.parameter.name.l should contain("x")
    }

    "testClosureWithExternalParameterName" in {
      val cpg = code("""
      |_ = { (_ x: MyType) in }
      |_ = { (x y: MyType) in }
      |""".stripMargin)
      val closureMethods = cpg.method.name("<lambda>.*").l
      closureMethods.name.l shouldBe List("<lambda>0", "<lambda>1")
      val closure0 = cpg.method.nameExact("<lambda>0").head
      closure0.parameter.name.l should contain("x")
      val closure1 = cpg.method.nameExact("<lambda>1").head
      closure1.parameter.name.l should contain("y")
    }

    "testClosureParameterWithAttribute" in {
      val cpg = code("""
      |_ = { (@_noImplicitCopy _ x: Int) -> () in }
      |_ = { (@Wrapper x) in }
      |withInvalidOrderings { (comparisonPredicate: @escaping (Int, Int) -> Bool) in }
      |""".stripMargin)
      val closureMethods = cpg.method.name("<lambda>.*").l
      closureMethods.name.l shouldBe List("<lambda>0", "<lambda>1", "<lambda>2")
      val closure0 = cpg.method.nameExact("<lambda>0").head
      closure0.parameter.name.l should contain("x")
      val closure2 = cpg.method.nameExact("<lambda>2").head
      closure2.parameter.name.l should contain("comparisonPredicate")
    }

    "testClosureWithDollarIdentifier" in {
      val cpg = code("""
          |let (ids, (actions, tracking)) = state.withCriticalRegion { ($0.valueObservers(for: keyPath), $0.didSet(keyPath: keyPath)) }
          |""".stripMargin)
      val closureMethods = cpg.method.name("<lambda>.*").l
      closureMethods.name.l shouldBe List("<lambda>0")
      cpg.identifier.name.toSet should contain("$0")
    }

    "testArrayExprWithNoCommas" in {
      val cpg                = code("[() ()]")
      val List(outerInit, _) = cpg.call.nameExact(Operators.arrayInitializer).l
      outerInit.code shouldBe "[() ()]"
    }

    "testDictionaryExprWithNoCommas" in {
      val cpg = code("""
      |[1: (), 2: ()]
      |["foo": 1, "bar": 2]
      |[1: "hello", 2: "world"]
      |""".stripMargin)
      val assignments = cpg.call.nameExact(Operators.assignment).l
      assignments.code.l shouldBe List(
        "<tmp>0[1] = ()",
        "<tmp>0[2] = ()",
        "<tmp>1[\"foo\"] = 1",
        "<tmp>1[\"bar\"] = 2",
        "<tmp>2[1] = \"hello\"",
        "<tmp>2[2] = \"world\""
      )
      val indexAccessCodes = cpg.call.nameExact(Operators.indexAccess).code.l
      indexAccessCodes shouldBe List(
        "<tmp>0[1]",
        "<tmp>0[2]",
        "<tmp>1[\"foo\"]",
        "<tmp>1[\"bar\"]",
        "<tmp>2[1]",
        "<tmp>2[2]"
      )
    }

    "inner text in literal strings" in {
      val tripQuote = "\"\"\""
      val cpg = code(s"""
           |class Foo {
           | var a = "abc";
           | var b = "\\\"abc";
           | var c = "abc\\\"";
           | var d = $tripQuote
           |abc
           |def
           |$tripQuote;
           |}
           |""".stripMargin)

      cpg.literal.strippedCode.l shouldBe List(
        "abc",
        "\\\"abc",
        "abc\\\"",
        "abc", // Multiline strings are split into single strings in Swift
        "def"
      )
    }
  }

}
