package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ExpressionTests extends AstSwiftSrc2CpgSuite {

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

    "testSequence2" ignore {
      val cpg = code("A as? B + C -> D is E as! F ? G = 42 : H")
      ???
    }

    "testClosureLiterals" ignore {
      val cpg = code("""
      |@MainActor (a: Int) async -> Int in print("hi")
      |{ @MainActor (a: Int) async -> Int in print("hi") }
      |{ [weak self, weak weakB = b] foo in
      |  return 0
      |}
      |""".stripMargin)
      ???
    }

    "testTrailingClosures" ignore {
      val cpg = code("""
      |var button =  View.Button[5, 4, 3] {
      |  // comment #0
      |  Text("ABC")
      |}
      |
      |compactMap { (parserDiag) in }
      |""".stripMargin)
      ???
    }

    "testNestedTypeSpecialization" ignore {
      val cpg = code("Swift.Array<Array<Foo>>()")
      ???
    }

    "testObjectLiterals" ignore {
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
      ???
    }

    "testKeypathExpression" ignore {
      val cpg = code("""
      |\a.b.c
      |\.?.foo
      |\ABCProtocol[100]
      |children.filter(\.type.defaultInitialization.isEmpty)
      |_ = \Lens<[Int]>.[0]
      |\S<T>.x
      |\TupleProperties.self
      |\Tuple<Int, Int>.self
      |\T.extension
      |\T.12[14]
      |\String?.!.count.?
      |\Optional.?!?!?!?
      |\Optional.?!?!?!?.??!
      |\(UnsafeRawPointer?, String).1
      |_ = distinctUntilChanged(\ .?.status)
      |_ = distinctUntilChanged(\.?.status)
      |""".stripMargin)
      ???
    }

    "testInitializerExpression" ignore {
      val cpg = code("Lexer.Cursor(input: input, previous: 0)")
      ???
    }

    "testCollectionLiterals" ignore {
      val cpg = code("""
      |[Dictionary<String, Int>: Int]()
      |[(Int, Double) -> Bool]()
      |[(Int, Double) -> Bool]()
      |_ = [@convention(block) ()  -> Int]().count
      |A<@convention(c) () -> Int32>.c()
      |A<(@autoclosure @escaping () -> Int, Int) -> Void>.c()
      |_ = [String: (@escaping (A<B>) -> Int) -> Void]().keys
      |
      |[
      |  condition ? firstOption : secondOption,
      |  bar(),
      |]
      |#fancyMacro<Arg1, Arg2>(hello: "me")
      |""".stripMargin)
      ???
    }

    "testInterpolatedStringLiterals" ignore {
      val cpg = code("""
      |return "Fixit: \(range.debugDescription) Text: \"\(text)\""
      |"text \(array.map({ "\($0)" }).joined(separator: ",")) text"
      |""".stripMargin)
      ???
    }

    "testStringLiterals1" ignore {
      val cpg = code(""""\(x)"""")
      ???
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

    "testMoveExpression" ignore {
      val cpg = code("""
      |_move msg
      |use(_move msg)
      |_move msg
      |let b = (_move self).buffer
      |""".stripMargin)
      ???
    }

    "testBorrowExpression" ignore {
      val cpg = code("""
      |_borrow msg
      |use(_borrow msg)
      |_borrow msg
      |let b = (_borrow self).buffer
      |""".stripMargin)
      ???
    }

    "testKeywordApplyExpression" ignore {
      val cpg = code("""
      |optional(x: .some(23))
      |optional(x: .none)
      |var pair : (Int, Double) = makePair(a: 1, b: 2.5)
      |""".stripMargin)
      ???
    }

    "testMacroExpansionExpression" ignore {
      val cpg = code("""
      |#file == $0.path
      |let a = #embed("filename.txt")
      |#Test {
      |  print("This is a test")
      |}
      |""".stripMargin)
      ???
    }

    "testIfExprInCoercion" ignore {
      val cpg = code("""
      |func foo() {
      |  if .random() { 0 } else { 1 } as Int
      |}
      |""".stripMargin)
      ???
    }

    "testSwitchExprInCoercion" ignore {
      val cpg = code("switch Bool.random() { case true: 0 case false: 1 } as Int")
      ???
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
            cndNode.code shouldBe ".random()"
          }
          controlStruct.whenTrue.code.l shouldBe List("0")
          controlStruct.whenFalse.code.l shouldBe List("1")
          controlStruct.lineNumber shouldBe Option(3)
          controlStruct.columnNumber shouldBe Option(10)
      }
    }

    "testSwitchExprInReturn" ignore {
      val cpg = code("""
      |func foo() {
      |  return switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testTryIf1" ignore {
      val cpg = code("""
      |func foo() -> Int {
      |  try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testTryIf2" ignore {
      val cpg = code("""
      |func foo() -> Int {
      |  return try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testTryIf3" ignore {
      val cpg = code("""
      |func foo() -> Int {
      |  let x = try if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin)
      ???
    }

    "testAwaitIf1" ignore {
      val cpg = code("""
      |func foo() async -> Int {
      |  await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testAwaitIf2" ignore {
      val cpg = code("""
      |func foo() async -> Int {
      |  return await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testAwaitIf3" ignore {
      val cpg = code("""
      |func foo() async -> Int {
      |  let x = await if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin)
      ???
    }

    "testTrySwitch1" ignore {
      val cpg = code("try switch Bool.random() { case true: 0 case false: 1 }")
      ???
    }

    "testTrySwitch2" ignore {
      val cpg = code("""
      |func foo() -> Int {
      |  return try switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testTrySwitch3" ignore {
      val cpg = code("""
      |func foo() -> Int {
      |  let x = try switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin)
      ???
    }

    "testAwaitSwitch1" ignore {
      val cpg = code("await switch Bool.random() { case true: 0 case false: 1 }")
      ???
    }

    "testAwaitSwitch2" ignore {
      val cpg = code("""
      |func foo() async -> Int {
      |  return await switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin)
      ???
    }

    "testAwaitSwitch3" ignore {
      val cpg = code("""
      |func foo() async -> Int {
      |  let x = await switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin)
      ???
    }

    "testIfExprCondCast" ignore {
      val cpg = code("if .random() { 0 } else { 1 } as? Int")
      ???
    }

    "testIfExprForceCast" ignore {
      val cpg = code("if .random() { 0 } else { 1 } as! Int")
      ???
    }

    "testSwitchExprCondCast" ignore {
      val cpg = code("switch Bool.random() { case true: 0 case false: 1 } as? Int")
      ???
    }

    "testSwitchExprForceCast" ignore {
      val cpg = code("switch Bool.random() { case true: 0 case false: 1 } as! Int")
      ???
    }

    "testInitCallInPoundIf" ignore {
      val cpg = code("""
      |class C {
      |  init() {
      |  #if true
      |    init()
      |  #endif
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testClosureParameterWithModifier" ignore {
      val cpg = code("_ = { (_const x: Int) in }")
      ???
    }

    "testClosureWithExternalParameterName" ignore {
      val cpg = code("""
      |_ = { (_ x: MyType) in }
      |_ = { (x y: MyType) in }
      |""".stripMargin)
      ???
    }

    "testClosureParameterWithAttribute" ignore {
      val cpg = code("""
      |_ = { (@_noImplicitCopy _ x: Int) -> () in }
      |_ = { (@Wrapper x) in }
      |withInvalidOrderings { (comparisonPredicate: @escaping (Int, Int) -> Bool) in }
      |""".stripMargin)
      ???
    }

    "testClosureWithDollarIdentifier" ignore {
      val cpg = code("""
      |let (ids, (actions, tracking)) = state.withCriticalRegion { ($0.valueObservers(for: keyPath), $0.didSet(keyPath: keyPath)) }
      |""".stripMargin)
      ???
    }

    "testArrayExprWithNoCommas" ignore {
      val cpg = code("[() ()]")
      ???
    }

    "testDictionaryExprWithNoCommas" ignore {
      val cpg = code("""
      |[1: (), 2: ()]
      |["foo": 1, "bar": 2]
      |[1: "hello", 2: "world"]
      |""".stripMargin)
      ???
    }

  }

}
