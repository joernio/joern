// This test file has been translated from swift/test/Parse/matching_patterns.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MatchingPatternsTests extends SwiftSrc2CpgSuite {

  "MatchingPatternsTests" should {

    "testMatchingPatterns6" in {
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
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch x")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List(
        "case 0",
        "case 1 + 2",
        "case square(9)",
        "case var a",
        "case let a",
        "case inout a",
        "case var var a",
        "case var let a",
        "case var (var b)",
        "case _",
        "case 1 + (_)"
      )
    }

    "testMatchingPatterns7" in {
      val cpg = code("""
      |switch (x,x) {
      |  case (var a, var a):
      |  fallthrough
      |  case _:
      |  ()
      |  }
      |  """.stripMargin)
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch (x,x)")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List("case (var a, var a)", "case _")
    }

    "testMatchingPatterns9" in {
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
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch e")
      val List(jt) = switchStructure.ast.collectAll[JumpTarget].l
      jt.name should startWith("case is Int")
    }

    "testMatchingPatterns10" in {
      val cpg            = code("enum Foo { case A, B, C }")
      val List(enumDecl) = cpg.typeDecl.nameExact("Foo").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Foo"
      enumDecl.member.name.l shouldBe List("A", "B", "C")
    }

    "testMatchingPatterns12" in {
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
      val List(enumDecl) = cpg.typeDecl.filename("Test0.swift").nameExact("Voluntary").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Voluntary"
      enumDecl.inheritsFromTypeFullName.l should contain("Equatable")
      enumDecl.member.name.l shouldBe List("Naught", "Mere", "Twain")
      enumDecl.method.name.l should contain("enumMethod")
      val switches = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switches.code.l.map(_.takeWhile(_ != '\n')) shouldBe List("switch self {", "switch foo {")
    }

    "testMatchingPatterns13" in {
      val cpg = code("""
      |var n : Voluntary<Int> = .Naught
      |if case let .Naught(value) = n {}
      |if case let .Naught(value1, value2, value3) = n {}
      |if case inout .Naught(value) = n {}
      |if case _mutating .Naught(value) = n {}
      |if case _borrowing .Naught(value) = n {}
      |if case _consuming .Naught(value) = n {}
      |""".stripMargin)
      val ifs = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifs.code.l.map(_.takeWhile(_ != '=').trim) shouldBe List(
        "if case let .Naught(value)",
        "if case let .Naught(value1, value2, value3)",
        "if case inout .Naught(value)",
        "if case _mutating .Naught(value)",
        "if case _borrowing .Naught(value)",
        "if case _consuming .Naught(value)"
      )
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      val List(localN)      = globalBlock.local.nameExact("n").l
      localN.typeFullName shouldBe "Voluntary"
    }

    "testMatchingPatterns14" in {
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
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch n")
      val jts        = switchStructure.ast.collectAll[JumpTarget].l
      val firstLines = jts.name.l.map(_.takeWhile(_ != '\n'))
      firstLines shouldBe List("case Foo.A", "case Voluntary<Int>.Naught,", "case Voluntary<Int>.Mere,", "case .Twain,")
    }

    "testMatchingPatterns16" in {
      val cpg = code("""
      |switch notAnEnum {
      |  case .Foo:
      |  ()
      |}
      |""".stripMargin)
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch notAnEnum")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List("case .Foo")
    }

    "testMatchingPatterns19" in {
      val cpg               = code("var m : ImportedEnum = .Simple")
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      val List(localM)      = globalBlock.local.nameExact("m").l
      localM.typeFullName shouldBe "ImportedEnum"
      val List(assign) = globalBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign.code shouldBe "var m: ImportedEnum = .Simple"
    }

    "testMatchingPatterns22" in {
      val cpg = code("""
      |enum LabeledScalarPayload {
      |  case Payload(name: Int)
      |}
      |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("LabeledScalarPayload").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.LabeledScalarPayload"
      enumDecl.member.name.l shouldBe List("Payload")
    }

    "testMatchingPatterns28" in {
      val cpg               = code("var t = (1, 2, 3)")
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      val List(localT)      = globalBlock.local.nameExact("t").l
      localT.code shouldBe "t"
      val List(assign) = globalBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign.code shouldBe "var t = (1, 2, 3)"
      val List(tupleCall) = assign.astChildren.isCall.l
      tupleCall.name shouldBe Operators.arrayInitializer
      tupleCall.argument.isLiteral.code.l shouldBe List("1", "2", "3")
    }

    "testMatchingPatterns32" in {
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
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch [Derived(), Derived(), Base()]")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List(
        "case let ds as [Derived]",
        "case is [Derived]",
        "default"
      )
    }

    "testMatchingPatterns33" in {
      val cpg = code("""
      |// Optional patterns.
      |let op1 : Int?
      |let op2 : Int??
      |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      val List(op1)         = globalBlock.local.nameExact("op1").l
      op1.typeFullName shouldBe "Swift.Int"
      val List(op2) = globalBlock.local.nameExact("op2").l
      op2.code shouldBe "op2"
    }

    "testMatchingPatterns34" in {
      val cpg = code("""
      |switch op1 {
      |  case nil: break
      |  case 1?: break
      |  case _?: break
      |}
      |""".stripMargin)
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch op1")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List("case nil", "case 1?", "case _?")
    }

    "testMatchingPatterns35" in {
      val cpg = code("""
      |switch op2 {
      |  case nil: break
      |  case _?: break
      |  case (1?)?: break
      |  case (_?)?: break
      |}
      |""".stripMargin)
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch op2")
      switchStructure.ast.collectAll[JumpTarget].name.l shouldBe List("case nil", "case _?", "case (1?)?", "case (_?)?")
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
      val List(ifNode)                      = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock                         = ifNode.condition.isBlock.l.head
      val tmpName                           = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      val List(_)                           = condBlock.ast.isLocal.nameExact("a").l
      val List(_)                           = condBlock.ast.isLocal.nameExact("b").l
      val List(tmpAssign, assignA, assignB) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      tmpAssign.code shouldBe s"$tmpName = x"
      tmpAssign.order shouldBe 1
      assignA.code shouldBe s"a = $tmpName.0"
      assignA.order shouldBe 2
      assignB.code shouldBe s"b = $tmpName.1"
      assignB.order shouldBe 3
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
      val List(ifNode)             = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock                = ifNode.condition.isBlock.l.head
      val tmpName                  = condBlock.ast.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      val List(_)                  = condBlock.ast.isLocal.nameExact("a").l
      val List(tmpAssign, assignA) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      tmpAssign.code shouldBe s"$tmpName = x"
      tmpAssign.order shouldBe 1
      assignA.code shouldBe s"a = $tmpName.0"
      assignA.order shouldBe 2
      val List(eqCall) = condBlock.astChildren.isCall.nameExact(Operators.equals).l
      eqCall.code shouldBe s"$tmpName.1 == 1"
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
      val List(_)           = methodBlock.local.nameExact("a").l
      val List(_)           = methodBlock.local.nameExact("b").l
      val List(guardIf)     = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val condBlock         = guardIf.condition.isBlock.l.head
      val tmpName           = condBlock.astChildren.isLocal.name.filter(_.startsWith("<tmp>")).loneElement
      val List(tmpAssign, assignA, assignB) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      tmpAssign.code shouldBe s"$tmpName = x"
      tmpAssign.order shouldBe 1
      assignA.code shouldBe s"a = $tmpName.0"
      assignA.order shouldBe 2
      assignB.code shouldBe s"b = $tmpName.1"
      assignB.order shouldBe 3
      val isTupleOp         = Defines.createIsTupleOperator(2)
      val List(isTupleCall) = condBlock.astChildren.isCall.nameExact(isTupleOp).l
      isTupleCall.code shouldBe s"$isTupleOp($tmpName)"
      isTupleCall.order shouldBe 4
      val List(retNode) = guardIf.astChildren.order(3).ast.isReturn.l
      retNode.code should startWith("return")
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
      val List(methodBlock) = cpg.method.nameExact("foo").block.l
      val List(tmpLocal)    = methodBlock.local.nameNot("self", "x").l
      val tmpName           = tmpLocal.name
      tmpName shouldBe "<tmp>0"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { (<tmp>0 = x) != nil }
      val List(condBlock) = ifNode.condition.isBlock.l
      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($tmpName = x) != nil"

      // Then block: { a = <tmp>0.0; b = <tmp>0.1; print(a); print(b) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(assignA) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      assignA.argument(1).code shouldBe "a"
      assignA.argument(2).code shouldBe s"$tmpName.0"

      val List(assignB) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      assignB.argument(1).code shouldBe "b"
      assignB.argument(2).code shouldBe s"$tmpName.1"

      val List(printA, printB) = thenBlock.astChildren.isCall.nameExact("print").l
      printA.code shouldBe "print(a)"
      printB.code shouldBe "print(b)"
    }

  }

}
