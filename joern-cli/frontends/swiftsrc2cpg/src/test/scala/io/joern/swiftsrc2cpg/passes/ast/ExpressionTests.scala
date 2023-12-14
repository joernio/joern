package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ExpressionTests extends AbstractPassTest {

  "ExpressionTests" should {
    "testTernary" in AstFixture("a ? b : c") { cpg =>
      inside(cpg.method.name("<global>").ast.isCall.l) { case List(call: Call) =>
        call.code shouldBe "a ? b : c"
        call.name shouldBe Operators.conditional
        inside(call.argument.l) { case List(cond, trueCase, falseCase) =>
          cond.code shouldBe "a"
          trueCase.code shouldBe "b"
          falseCase.code shouldBe "c"
        }
        call.lineNumber shouldBe Some(1)
        call.columnNumber shouldBe Some(1)
      }
    }

    "testSequence1" in AstFixture("a ? b : c ? d : e") { cpg =>
      inside(cpg.method.name("<global>").ast.isCall.l) { case List(call: Call, nestedCall: Call) =>
        call.code shouldBe "a ? b : c ? d : e"
        call.name shouldBe Operators.conditional
        inside(call.argument.l) { case List(cond, trueCase, falseCase: Call) =>
          cond.code shouldBe "a"
          trueCase.code shouldBe "b"
          falseCase.code shouldBe "c ? d : e"
          falseCase shouldBe nestedCall
          inside(falseCase.argument.l) { case List(cond, trueCase, falseCase) =>
            cond.code shouldBe "c"
            trueCase.code shouldBe "d"
            falseCase.code shouldBe "e"
          }
        }
      }
    }

    "testSequence2" ignore AstFixture("A as? B + C -> D is E as! F ? G = 42 : H") { cpg => ??? }

    "testClosureLiterals" ignore AstFixture("""
      |@MainActor (a: Int) async -> Int in print("hi")
      |{ @MainActor (a: Int) async -> Int in print("hi") }
      |{ [weak self, weak weakB = b] foo in
      |  return 0
      |}
      |""".stripMargin) { cpg => ??? }

    "testTrailingClosures" ignore AstFixture("""
      |var button =  View.Button[5, 4, 3] {
      |  // comment #0
      |  Text("ABC")
      |}
      |
      |compactMap { (parserDiag) in }
      |""".stripMargin) { cpg => ??? }

    "testNestedTypeSpecialization" ignore AstFixture("Swift.Array<Array<Foo>>()") { cpg => ??? }

    "testObjectLiterals" ignore AstFixture("""
      |#colorLiteral1()
      |#colorLiteral2(red: 1.0)
      |#colorLiteral3(red: 1.0, green: 1.0)
      |#colorLiteral4(red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0)
      |#imageLiteral1()
      |#imageLiteral2(resourceName: "foo.png")
      |#imageLiteral3(resourceName: "foo/bar/baz/qux.png")
      |#imageLiteral4(resourceName: "foo/bar/baz/quux.png")
      |""".stripMargin) { cpg => ??? }

    "testKeypathExpression" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testInitializerExpression" ignore AstFixture("Lexer.Cursor(input: input, previous: 0)") { cpg => ??? }

    "testCollectionLiterals" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testInterpolatedStringLiterals" ignore AstFixture("""
      |return "Fixit: \(range.debugDescription) Text: \"\(text)\""
      |"text \(array.map({ "\($0)" }).joined(separator: ",")) text"
      |""".stripMargin) { cpg => ??? }

    "testStringLiterals" ignore AstFixture(""""\(x)"""") { cpg => ??? }

    "testMoveExpression" ignore AstFixture("""
      |_move msg
      |use(_move msg)
      |_move msg
      |let b = (_move self).buffer
      |""".stripMargin) { cpg => ??? }

    "testBorrowExpression" ignore AstFixture("""
      |_borrow msg
      |use(_borrow msg)
      |_borrow msg
      |let b = (_borrow self).buffer
      |""".stripMargin) { cpg => ??? }

    "testKeywordApplyExpression" ignore AstFixture("""
      |optional(x: .some(23))
      |optional(x: .none)
      |var pair : (Int, Double) = makePair(a: 1, b: 2.5)
      |""".stripMargin) { cpg => ??? }

    "testMacroExpansionExpression" ignore AstFixture("""
      |#file == $0.path
      |let a = #embed("filename.txt")
      |#Test {
      |  print("This is a test")
      |}
      |""".stripMargin) { cpg => ??? }

    "testIfExprInCoercion" ignore AstFixture("""
      |func foo() {
      |  if .random() { 0 } else { 1 } as Int
      |}
      |""".stripMargin) { cpg => ??? }

    "testSwitchExprInCoercion" ignore AstFixture("switch Bool.random() { case true: 0 case false: 1 } as Int") { cpg =>
      ???
    }

    "testIfExprInReturn" in AstFixture("""
      |func foo() {
      |  return if .random() { 0 } else { 1 }
      |}
      |""".stripMargin) { cpg =>
      inside(cpg.method.name("foo").ast.isReturn.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("if .random() {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.IF
          inside(controlStruct.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe ".random()"
          }
          controlStruct.whenTrue.code.l shouldBe List("0")
          controlStruct.whenFalse.code.l shouldBe List("1")
          controlStruct.lineNumber shouldBe Some(3)
          controlStruct.columnNumber shouldBe Some(10)
      }
    }

    "testSwitchExprInReturn" ignore AstFixture("""
      |func foo() {
      |  return switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testTryIf1" ignore AstFixture("""
      |func foo() -> Int {
      |  try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testTryIf2" ignore AstFixture("""
      |func foo() -> Int {
      |  return try if .random() { 0 } else { 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testTryIf3" ignore AstFixture("""
      |func foo() -> Int {
      |  let x = try if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin) { cpg => ??? }

    "testAwaitIf1" ignore AstFixture("""
      |func foo() async -> Int {
      |  await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testAwaitIf2" ignore AstFixture("""
      |func foo() async -> Int {
      |  return await if .random() { 0 } else { 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testAwaitIf3" ignore AstFixture("""
      |func foo() async -> Int {
      |  let x = await if .random() { 0 } else { 1 }
      |  return x
      |}
      |""".stripMargin) { cpg => ??? }

    "testTrySwitch1" ignore AstFixture("try switch Bool.random() { case true: 0 case false: 1 }") { cpg => ??? }

    "testTrySwitch2" ignore AstFixture("""
      |func foo() -> Int {
      |  return try switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testTrySwitch3" ignore AstFixture("""
      |func foo() -> Int {
      |  let x = try switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin) { cpg => ??? }

    "testAwaitSwitch1" ignore AstFixture("await switch Bool.random() { case true: 0 case false: 1 }") { cpg => ??? }

    "testAwaitSwitch2" ignore AstFixture("""
      |func foo() async -> Int {
      |  return await switch Bool.random() { case true: 0 case false: 1 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testAwaitSwitch3" ignore AstFixture("""
      |func foo() async -> Int {
      |  let x = await switch Bool.random() { case true: 0 case false: 1 }
      |  return x
      |}
      |""".stripMargin) { cpg => ??? }

    "testIfExprCondCast" ignore AstFixture("if .random() { 0 } else { 1 } as? Int") { cpg => ??? }

    "testIfExprForceCast" ignore AstFixture("if .random() { 0 } else { 1 } as! Int") { cpg => ??? }

    "testSwitchExprCondCast" ignore AstFixture("switch Bool.random() { case true: 0 case false: 1 } as? Int") { cpg =>
      ???
    }

    "testSwitchExprForceCast" ignore AstFixture("switch Bool.random() { case true: 0 case false: 1 } as! Int") { cpg =>
      ???
    }

    "testInitCallInPoundIf" ignore AstFixture("""
      |class C {
      |  init() {
      |  #if true
      |    init()
      |  #endif
      |  }
      |}
      |""".stripMargin) { cpg => ??? }

    "testClosureParameterWithModifier" ignore AstFixture("_ = { (_const x: Int) in }") { cpg => ??? }

    "testClosureWithExternalParameterName" ignore AstFixture("""
      |_ = { (_ x: MyType) in }
      |_ = { (x y: MyType) in }
      |""".stripMargin) { cpg => ??? }

    "testClosureParameterWithAttribute" ignore AstFixture("""
      |_ = { (@_noImplicitCopy _ x: Int) -> () in }
      |_ = { (@Wrapper x) in }
      |withInvalidOrderings { (comparisonPredicate: @escaping (Int, Int) -> Bool) in }
      |""".stripMargin) { cpg => ??? }

    "testClosureWithDollarIdentifier" ignore AstFixture("""
      |let (ids, (actions, tracking)) = state.withCriticalRegion { ($0.valueObservers(for: keyPath), $0.didSet(keyPath: keyPath)) }
      |""".stripMargin) { cpg => ??? }

    "testArrayExprWithNoCommas" ignore AstFixture("[() ()]") { cpg => ??? }

    "testDictionaryExprWithNoCommas" ignore AstFixture("""
      |[1: (), 2: ()]
      |["foo": 1, "bar": 2]
      |[1: "hello", 2: "world"]
      |""".stripMargin) { cpg => ??? }

  }

}
