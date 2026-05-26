package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class StatementTests extends SwiftSrc2CpgSuite {

  "StatementTests" should {

    "testIfLet" in {
      val cpg = code("""
      |func test() {
      |  if let baz = optionalValue {
      |    print(baz)
      |  }
      |}
      |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: desugared to { <tmp>0 = optionalValue; <tmp>0 != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      tmpLocal.name should startWith("<tmp>")
      val tmpName = tmpLocal.name

      val List(condAssign) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      condAssign.code shouldBe s"$tmpName = optionalValue"
      condAssign.argument(1).code shouldBe tmpName
      condAssign.argument(2).code shouldBe "optionalValue"

      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"$tmpName != nil"
      condCheck.argument(1).code shouldBe tmpName
      condCheck.argument(2).code shouldBe "nil"

      // Then block: { let baz = <tmp>0; print(baz) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(bazLocal) = thenBlock.astChildren.isLocal.l
      bazLocal.name shouldBe "baz"

      val List(thenAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      thenAssign.code shouldBe s"baz = $tmpName"
      thenAssign.argument(1).code shouldBe "baz"
      thenAssign.argument(2).code shouldBe tmpName
    }

    "testDoCatch" in {
      val cpg = code("""
      |do {
      |  try foo()
      |} catch {
      |  bar()
      |}
      |do { try foo() }
      |catch where (error as NSError) == NSError() {}
      |""".stripMargin)
      val List(doStructure1, doStructure2) =
        cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY).code("do \\{.*").l

      val List(fooCall1) = doStructure1.astChildren.order(1).isCall.l
      fooCall1.code shouldBe "foo()"
      val List(catchBlock1) = doStructure1.astChildren.isControlStructure.isCatch.l
      catchBlock1.order shouldBe 2
      catchBlock1.astChildren.isCall.codeExact("bar()").size shouldBe 1

      val List(fooCall2) = doStructure2.astChildren.order(1).isCall.l
      fooCall2.code shouldBe "foo()"
      val List(catchBlock2) = doStructure2.astChildren.isControlStructure.isCatch.l
      catchBlock2.order shouldBe 2
      catchBlock2.astChildren.isCall.code.l shouldBe List("(error as NSError) == NSError()")
    }

    "testReturn" in {
      val cpg = code("""
      |return actor
      |{ return 0 }
      |return
      |""".stripMargin)
      // Top-level `return` statements + a closure literal yielding `return 0`. We expect
      // a `<lambda>0` method to be created for the closure body; the bare `{ return 0 }`
      // closure is wrapped via `single_apply`.
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.<lambda>0:()->ANY",
        "Swift.Function<()->ANY>.single_apply:()->ANY"
      )
      val returnCodes = cpg.method.ast.isReturn.code.l
      // The `return actor` line is followed by a closure literal `{ return 0 }` on the next
      // line — Swift parses the trailing closure as the return expression, so the outer
      // return's code spans both lines.
      returnCodes should contain allOf ("return actor\n{ return 0 }", "return 0", "return")
    }

    "testMissingIfClauseIntroducer" in {
      // Invalid Swift (`if _ = 42`); the parser still recovers an IF control structure.
      val cpg          = code("if _ = 42 {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if")
    }

    "testIfHasSymbol" in {
      val cpg = code("""
      |if #_hasSymbol(foo) {}
      |if #_hasSymbol(foo as () -> ()) {}
      |""".stripMargin)
      // `#_hasSymbol` is an availability-style predicate. The IF control structures must still be created.
      val ifs = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifs.code.l shouldBe List("if #_hasSymbol(foo) {}", "if #_hasSymbol(foo as () -> ()) {}")
    }

    "testYield" in {
      val cpg = code("""
      |var x: Int {
      |  _read {
      |    yield &x
      |  }
      |}
      |func f() -> Int {
      |  yield 5
      |}
      |""".stripMargin)
      // The `_read` accessor on `var x` is materialized as a method named `_read` whose
      // qualifier is the property name `x`, returning Swift.Int. `func f` is a sibling.
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.x._read:Swift.Int",
        "Test0.swift:<global>.f:()->Swift.Int"
      )
      // `yield &x` lowers to an addressOf operator; the `&x` argument shows up as a call.
      cpg.call.nameExact(Operators.addressOf).code.l should contain("&x")
    }

    "testTrailingClosureInIfCondition" in {
      // Edge case the parser disambiguates: `if test { $0 } {}` — `{ $0 }` is a closure
      // argument to `test`, the outer `{}` is the IF body. We just lock in that an IF survives.
      val cpg          = code("if test { $0 } {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if test")
    }

    "testClosureInsideIfCondition" in {
      // Invalid Swift (`if true, {x}() {}`). Parser recovers; assert the IF survives.
      val cpg          = code("if true, {x}() {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if true")
    }

    "testIfLetWithoutInitializer" in {
      val cpg = code("""
      |func test(optionalValue: Int?) {
      |  if let optionalValue {
      |    print(optionalValue)
      |  }
      |}
      |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // For "if let optionalValue" without explicit initializer:
      // Condition: optionalValue != nil (direct comparison, no block)
      val List(condCheck) = ifNode.condition.isCall.l
      condCheck.name shouldBe Operators.notEquals
      condCheck.code shouldBe "optionalValue != nil"
      condCheck.argument(1).code shouldBe "optionalValue"
      condCheck.argument(2).code shouldBe "nil"

      // Then branch: just the original body, no new local or assignment for optionalValue
      // The body might be a block or another structure depending on CodeBlockSyntax handling
      val thenNodes = ifNode.whenTrue.l
      thenNodes should not be empty

      // Verify no new local named "optionalValue" was created in the then branch
      val localsInThen = thenNodes.flatMap(_.ast.isLocal.nameExact("optionalValue").l)
      localsInThen shouldBe empty

      // Verify no assignment to optionalValue in the then branch
      val assignmentsInThen        = thenNodes.flatMap(_.ast.isCall.nameExact(Operators.assignment).l)
      val optionalValueAssignments = assignmentsInThen.filter(_.argument(1).code == "optionalValue")
      optionalValueAssignments shouldBe empty
    }

    "testWhileLet" in {
      val cpg = code("""
      |func test() {
      |  while let item = iterator.next() {
      |    print(item)
      |  }
      |}
      |""".stripMargin)

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: desugared to { <tmp>0 = iterator.next(); <tmp>0 != nil }
      val List(condBlock) = whileNode.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      tmpLocal.name should startWith("<tmp>")
      val tmpName = tmpLocal.name

      val List(condAssign) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      condAssign.code shouldBe s"$tmpName = iterator.next()"
      condAssign.argument(1).code shouldBe tmpName
      condAssign.argument(2).code shouldBe "iterator.next()"

      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"$tmpName != nil"
      condCheck.argument(1).code shouldBe tmpName
      condCheck.argument(2).code shouldBe "nil"

      // Loop body: { let item = <tmp>0; print(item) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      val List(itemLocal) = bodyBlock.astChildren.isLocal.l
      itemLocal.name shouldBe "item"

      val List(bodyAssign) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).l
      bodyAssign.code shouldBe s"item = $tmpName"
      bodyAssign.argument(1).code shouldBe "item"
      bodyAssign.argument(2).code shouldBe tmpName
    }

    "testWhileLetWithoutInitializer" in {
      val cpg = code("""
      |func test(optionalValue: Int?) {
      |  while let optionalValue {
      |    print(optionalValue)
      |  }
      |}
      |""".stripMargin)

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // For "while let optionalValue" without explicit initializer:
      // Condition: optionalValue != nil (direct comparison, no block)
      val List(condCheck) = whileNode.condition.isCall.l
      condCheck.name shouldBe Operators.notEquals
      condCheck.code shouldBe "optionalValue != nil"
      condCheck.argument(1).code shouldBe "optionalValue"
      condCheck.argument(2).code shouldBe "nil"

      // Loop body: just the original body, no new local or assignment for optionalValue
      val bodyNodes = whileNode.whenTrue.l
      bodyNodes should not be empty

      // Verify no new local named "optionalValue" was created in the loop body
      val localsInBody = bodyNodes.flatMap(_.ast.isLocal.nameExact("optionalValue").l)
      localsInBody shouldBe empty

      // Verify no assignment to optionalValue in the loop body
      val assignmentsInBody        = bodyNodes.flatMap(_.ast.isCall.nameExact(Operators.assignment).l)
      val optionalValueAssignments = assignmentsInBody.filter(_.argument(1).code == "optionalValue")
      optionalValueAssignments shouldBe empty
    }

    "testIfLetMultiple" in {
      val cpg = code("""
      |func test() {
      |  if let a = Optional(1), let b = Optional(2) {
      |    print(a, b)
      |  }
      |}
      |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { <tmp>0 = Optional(1); <tmp>1 = Optional(2); <tmp>0 != nil && <tmp>1 != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(tmp1Local, tmp2Local) = condBlock.astChildren.isLocal.l
      tmp1Local.name should startWith("<tmp>")
      tmp2Local.name should startWith("<tmp>")
      val tmp1Name = tmp1Local.name
      val tmp2Name = tmp2Local.name

      val List(assign1, assign2) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign1.code shouldBe s"$tmp1Name = Optional(1)"
      assign2.code shouldBe s"$tmp2Name = Optional(2)"

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"$tmp1Name != nil"
      check2.code shouldBe s"$tmp2Name != nil"

      // Then block: { a = <tmp>0; b = <tmp>1; print(a, b) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aLocal, bLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"
      bLocal.name shouldBe "b"

      val List(unwrapA, unwrapB) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp1Name"
      unwrapB.code shouldBe s"b = $tmp2Name"
    }

    "testIfLetMixed" in {
      val cpg = code("""
      |func test(opt2: Int?) {
      |  if let a = Optional(1), let opt2 {
      |    print(a, opt2)
      |  }
      |}
      |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { <tmp>0 = Optional(1); <tmp>0 != nil && opt2 != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(tmp1Local) = condBlock.astChildren.isLocal.l
      val tmp1Name        = tmp1Local.name

      val List(assign1) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign1.code shouldBe s"$tmp1Name = Optional(1)"

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"$tmp1Name != nil"
      check2.code shouldBe "opt2 != nil"

      // Then block: { a = <tmp>0; print(a, opt2) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"

      val List(unwrapA) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp1Name"
    }

    "testWhileLetMultiple" in {
      val cpg = code("""
      |func test() {
      |  while let a = iterator1.next(), let b = iterator2.next() {
      |    print(a, b)
      |  }
      |}
      |""".stripMargin)

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: { <tmp>0 = iterator1.next(); <tmp>1 = iterator2.next(); <tmp>0 != nil && <tmp>1 != nil }
      val List(condBlock) = whileNode.condition.isBlock.l

      val List(tmp1Local, tmp2Local) = condBlock.astChildren.isLocal.l
      val tmp1Name                   = tmp1Local.name
      val tmp2Name                   = tmp2Local.name

      val List(assign1, assign2) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign1.code shouldBe s"$tmp1Name = iterator1.next()"
      assign2.code shouldBe s"$tmp2Name = iterator2.next()"

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"$tmp1Name != nil"
      check2.code shouldBe s"$tmp2Name != nil"

      // Loop body: { a = <tmp>0; b = <tmp>1; print(a, b) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      val List(aLocal, bLocal) = bodyBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"
      bLocal.name shouldBe "b"

      val List(unwrapA, unwrapB) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp1Name"
      unwrapB.code shouldBe s"b = $tmp2Name"
    }

    "testIfLetMixedWithTuplePattern" in {
      val cpg = code("""
      |func test() {
      |  if let a = foo(), let (b, c) = bar() {
      |    print(a, b, c)
      |  }
      |}
      |""".stripMargin)

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { <tmp>0 = foo(); <tmp>0 != nil } (tuple pattern excluded from condition)
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(tmp1Local) = condBlock.astChildren.isLocal.l
      val tmp1Name        = tmp1Local.name

      val List(assign1) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign1.code shouldBe s"$tmp1Name = foo()"
      assign1.argument(1).code shouldBe tmp1Name
      assign1.argument(2).code shouldBe "foo()"

      val List(check1) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"$tmp1Name != nil"
      check1.argument(1).code shouldBe tmp1Name
      check1.argument(2).code shouldBe "nil"

      // Then block: { a = <tmp>0; let (b, c) = bar(); print(a, b, c) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      // First child: unwrapping assignment for 'a'
      val List(aLocal) = thenBlock.astChildren.isLocal.nameExact("a").l
      aLocal.name shouldBe "a"

      val List(unwrapA) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmp1Name").l
      unwrapA.argument(1).code shouldBe "a"
      unwrapA.argument(2).code shouldBe tmp1Name

      // Second child: tuple binding block for (b, c) = bar()
      // The tuple binding creates a nested block with tmp assignment + tuple destructuring
      val List(tupleBindingBlock) = thenBlock.astChildren.isBlock.l

      // Inside the tuple binding block, there should be an assignment involving bar()
      val List(barAssignment) = tupleBindingBlock.astChildren.isCall.nameExact(Operators.assignment).code(".*bar.*").l
      barAssignment.code should include("bar()")
    }

    "testWhileLetMixedWithTuplePattern" in {
      val cpg = code("""
      |func test() {
      |  while let a = foo(), let (b, c) = bar() {
      |    print(a, b, c)
      |  }
      |}
      |""".stripMargin)

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: { <tmp>0 = foo(); <tmp>0 != nil } (tuple pattern excluded from condition)
      val List(condBlock) = whileNode.condition.isBlock.l

      val List(tmp1Local) = condBlock.astChildren.isLocal.l
      val tmp1Name        = tmp1Local.name

      val List(assign1) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      assign1.code shouldBe s"$tmp1Name = foo()"
      assign1.argument(1).code shouldBe tmp1Name
      assign1.argument(2).code shouldBe "foo()"

      val List(check1) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"$tmp1Name != nil"
      check1.argument(1).code shouldBe tmp1Name
      check1.argument(2).code shouldBe "nil"

      // Loop body: { a = <tmp>0; let (b, c) = bar(); print(a, b, c) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      // First child: unwrapping assignment for 'a'
      val List(aLocal) = bodyBlock.astChildren.isLocal.nameExact("a").l
      aLocal.name shouldBe "a"

      val List(unwrapA) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmp1Name").l
      unwrapA.argument(1).code shouldBe "a"
      unwrapA.argument(2).code shouldBe tmp1Name

      // Second child: tuple binding block for (b, c) = bar()
      // The tuple binding creates a nested block with tmp assignment + tuple destructuring
      val List(tupleBindingBlock) = bodyBlock.astChildren.isBlock.l

      // Inside the tuple binding block, there should be an assignment involving bar()
      val List(barAssignment) = tupleBindingBlock.astChildren.isCall.nameExact(Operators.assignment).code(".*bar.*").l
      barAssignment.code should include("bar()")
    }

  }

}
