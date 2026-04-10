// This test file has been translated from swift/test/Parse/matching_patterns.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class MatchingPatternsTests extends SwiftSrc2CpgSuite {

  "MatchingPatternsTests" should {

    "testMatchingPatterns6" ignore {
      val cpg = code("""
      |switch x {
      |  // Expressions as patterns.
      |  case 0:
      |  ()
      |  case 1 + 2:
      |  ()
      |  case square(9):
      |  ()
      |  // 'var' and 'let' patterns.
      |  case var a:
      |  a = 1
      |  case let a:
      |  a = 1
      |  case inout a:
      |  a = 1
      |  case var var a:
      |  a += 1
      |  case var let a:
      |  print(a, terminator: "")
      |  case var (var b):
      |  b += 1
      |  // 'Any' pattern.
      |  case _:
      |  ()
      |  // patterns are resolved in expression-only positions are errors.
      |  case 1 + (_):
      |  ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns7" ignore {
      val cpg = code("""
      |switch (x,x) {
      |  case (var a, var a):
      |  fallthrough
      |  case _:
      |  ()
      |  }
      |  """.stripMargin)
      ???
    }

    "testMatchingPatterns9" ignore {
      val cpg = code("""
      |switch e {
      |  // 'is' pattern.
      |  case is Int,
      |   is A<Int>,
      |   is A<Int>.C<Int>,
      |   is (Int, Int),
      |   is (a: Int, b: Int):
      |   ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns10" ignore {
      val cpg = code("enum Foo { case A, B, C }")
      ???
    }

    "testMatchingPatterns12" ignore {
      val cpg = code("""
      |enum Voluntary<T> : Equatable {
      |  case Naught
      |  case Mere(T)
      |  case Twain(T, T)
      |  func enumMethod(_ other: Voluntary<T>, foo: Foo) {
      |  switch self {
      |  case other:
      |  ()
      |  case .Naught,
      |   .Naught(),
      |   .Naught(_),
      |   .Naught(_, _):
      |  ()
      |  case .Mere,
      |   .Mere(),
      |   .Mere(_),
      |   .Mere(_, _):
      |  ()
      |  case .Twain(),
      |   .Twain(_),
      |   .Twain(_, _),
      |   .Twain(_, _, _):
      |  ()
      |  }
      |  switch foo {
      |  case .Naught:
      |  ()
      |  case .A, .B, .C:
      |  ()
      |  }
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns13" ignore {
      val cpg = code("""
      |var n : Voluntary<Int> = .Naught
      |if case let .Naught(value) = n {}
      |if case let .Naught(value1, value2, value3) = n {}
      |if case inout .Naught(value) = n {}
      |if case _mutating .Naught(value) = n {}
      |if case _borrowing .Naught(value) = n {}
      |if case _consuming .Naught(value) = n {}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns14" ignore {
      val cpg = code("""
      |switch n {
      |  case Foo.A:
      |  ()
      |  case Voluntary<Int>.Naught,
      |   Voluntary<Int>.Naught(),
      |   Voluntary<Int>.Naught(_, _),
      |   Voluntary.Naught,
      |   .Naught:
      |  ()
      |  case Voluntary<Int>.Mere,
      |   Voluntary<Int>.Mere(_),
      |   Voluntary<Int>.Mere(_, _),
      |   Voluntary.Mere,
      |   Voluntary.Mere(_),
      |   .Mere,
      |   .Mere(_):
      |  ()
      |  case .Twain,
      |   .Twain(_),
      |   .Twain(_, _),
      |   .Twain(_, _, _):
      |  ()
      |  }
      |  """.stripMargin)
      ???
    }

    "testMatchingPatterns16" ignore {
      val cpg = code("""
      |switch notAnEnum {
      |  case .Foo:
      |  ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns19" ignore { val cpg = code("var m : ImportedEnum = .Simple") }

    "testMatchingPatterns22" ignore {
      val cpg = code("""
      |enum LabeledScalarPayload {
      |  case Payload(name: Int)
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns28" ignore { val cpg = code("var t = (1, 2, 3)") }

    "testMatchingPatterns32" ignore {
      val cpg = code("""
      |switch [Derived(), Derived(), Base()] {
      |  case let ds as [Derived]:
      |  ()
      |  case is [Derived]:
      |  ()
      |  default:
      |  ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns33" ignore {
      val cpg = code("""
      |// Optional patterns.
      |let op1 : Int?
      |let op2 : Int??
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns34" ignore {
      val cpg = code("""
      |switch op1 {
      |  case nil: break
      |  case 1?: break
      |  case _?: break
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns35" ignore {
      val cpg = code("""
      |switch op2 {
      |  case nil: break
      |  case _?: break
      |  case (1?)?: break
      |  case (_?)?: break
      |}
      |""".stripMargin)
      ???
    }

    "testIfCaseExpressionTuplePattern" in {
      val cpg = code("""
        |func foo(x: (Int, Int)) {
        |  if case (1, 2) = x {
        |    print("matched")
        |  }
        |}
        |""".stripMargin)
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock    = ifNode.condition.isBlock.l.head
      // Temp local
      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      val tmpName        = tmpLocal.name
      // Temp assignment
      val List(tmpAssign) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      tmpAssign.code shouldBe s"$tmpName = x"
      tmpAssign.order shouldBe 1
      // Equality chain
      val List(andCall) = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      andCall.code shouldBe s"$tmpName.0 == 1 && $tmpName.1 == 2"
      andCall.order shouldBe 2
      val List(eq1, eq2) = andCall.argument.isCall.nameExact(Operators.equals).l
      eq1.code shouldBe s"$tmpName.0 == 1"
      eq2.code shouldBe s"$tmpName.1 == 2"
      // isTuple check
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 3
    }

    "testIfCaseBindingTuplePattern" in {
      val cpg = code("""
        |func foo(x: (Int, Int)) {
        |  if case let (a, b) = x {
        |    print(a)
        |    print(b)
        |  }
        |}
        |""".stripMargin)
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock    = ifNode.condition.isBlock.l.head
      // Temp local + binding locals
      val tmpName = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      condBlock.ast.isLocal.nameExact("a").size shouldBe 1
      condBlock.ast.isLocal.nameExact("b").size shouldBe 1
      // Temp assignment
      val allAssigns = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      allAssigns.size shouldBe 3
      allAssigns.head.code shouldBe s"$tmpName = x"
      allAssigns.head.order shouldBe 1
      // Binding assignments with field accesses
      val List(assignA) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      val List(assignB) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      assignA.order shouldBe 2
      assignB.order shouldBe 3
      // isTuple check
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 4
    }

    "testIfCaseMixedTuplePattern" in {
      val cpg = code("""
        |func foo(x: (Int, Int)) {
        |  if case (let a, 1) = x {
        |    print(a)
        |  }
        |}
        |""".stripMargin)
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock    = ifNode.condition.isBlock.l.head
      // Temp local + binding local for a
      val tmpName = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      condBlock.ast.isLocal.nameExact("a").size shouldBe 1
      // Temp assignment
      val allAssigns = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      allAssigns.head.code shouldBe s"$tmpName = x"
      allAssigns.head.order shouldBe 1
      // Binding assignment for a
      val List(assignA) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      assignA.order shouldBe 2
      // Equality check for second element
      val List(eqCall) = condBlock.astChildren.isCall.nameExact(Operators.equals).l
      eqCall.code shouldBe s"$tmpName.1 == 1"
      // isTuple check
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
    }

    "testGuardCaseBindingTuplePattern" in {
      val cpg = code("""
        |func foo(x: (Int, Int)) {
        |  guard case let (a, b) = x else { return }
        |  print(a)
        |  print(b)
        |}
        |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("foo").block.l
      // Locals for a and b should be under the enclosing method block (guard scoping)
      methodBlock.local.nameExact("a").size shouldBe 1
      methodBlock.local.nameExact("b").size shouldBe 1
      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock     = guardIf.condition.isBlock.l.head
      // Temp local
      val tmpName = condBlock.astChildren.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      // Temp + binding assignments
      val allAssigns = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      allAssigns.size shouldBe 3
      allAssigns.head.code shouldBe s"$tmpName = x"
      allAssigns.head.order shouldBe 1
      // Binding assignments with field accesses
      val List(assignA) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      val List(assignB) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      assignA.order shouldBe 2
      assignB.order shouldBe 3
      // isTuple check
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 4
      // Guard: else branch contains the return
      guardIf.astChildren.order(3).ast.isReturn.size shouldBe 1
    }

    "testIfCaseNestedBindingTuplePattern" in {
      val cpg = code("""
        |func foo(x: ((Int, Int), Int)) {
        |  if case let ((a, b), c) = x {
        |    print(a)
        |  }
        |}
        |""".stripMargin)
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock    = ifNode.condition.isBlock.l.head
      // Tmp local + binding locals
      val tmpName = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      condBlock.ast.isLocal.name.toSet shouldBe Set("a", "b", "c", tmpName)
      // Temp assignment
      val allAssigns = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      allAssigns.head.code shouldBe s"$tmpName = x"
      allAssigns.head.order shouldBe 1
      // Nested field access: a = <tmp>.0.0, b = <tmp>.0.1, c = <tmp>.1
      val List(assignA) =
        condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0.0").l
      val List(assignB) =
        condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.0.1").l
      val List(assignC) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"c = $tmpName.1").l
      assignA.order shouldBe 2
      assignB.order shouldBe 3
      assignC.order shouldBe 4
      // isTuple check (outer tuple has 2 elements)
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 5
    }

    "testIfLetTuplePattern" in {
      val cpg = code("""
        |func foo(x: (Int, Int)?) {
        |  if let (a, b) = x {
        |    print(a)
        |    print(b)
        |  }
        |}
        |""".stripMargin)
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock    = ifNode.condition.isBlock.l.head
      // Tmp local + binding locals
      val tmpName = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      condBlock.ast.isLocal.nameExact("a").size shouldBe 1
      condBlock.ast.isLocal.nameExact("b").size shouldBe 1
      // Temp + binding assignments
      val allAssigns = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      allAssigns.size shouldBe 3
      allAssigns.head.code shouldBe s"$tmpName = x"
      allAssigns.head.order shouldBe 1
      // Binding assignments with field accesses
      val List(assignA) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      val List(assignB) = condBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      assignA.order shouldBe 2
      assignB.order shouldBe 3
      // isTuple check
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 4
    }

  }

}
