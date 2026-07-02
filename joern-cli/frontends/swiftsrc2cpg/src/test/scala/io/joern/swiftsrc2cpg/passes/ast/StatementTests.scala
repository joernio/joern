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
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // After desugaring: <tmp>0 in method block
      val List(tmpLocal, optionalValueLocal) = methodBlock.local.l
      val tmpName                            = tmpLocal.name
      tmpName shouldBe "<tmp>0"
      optionalValueLocal.name shouldBe "optionalValue"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: desugared to { (<tmp>0 = optionalValue) != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($tmpName = optionalValue) != nil"

      val List(condAssign) = condCheck.argument.assignment.l
      condAssign.code shouldBe s"$tmpName = optionalValue"
      condAssign.argument(1).code shouldBe tmpName
      condAssign.argument(2).code shouldBe "optionalValue"

      // Then block: { let baz = <tmp>0; print(baz) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(bazLocal) = thenBlock.astChildren.isLocal.l
      bazLocal.name shouldBe "baz"

      val List(thenAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      thenAssign.code shouldBe s"baz = $tmpName"
      thenAssign.argument(1).code shouldBe "baz"
      thenAssign.argument(2).code shouldBe tmpName
    }

    "testIfLetAfterWildcardGuard" in {
      val cpg = code("""
      |func foo(_ x: Int?) {
      |  guard let _ = x else { return }
      |  if let y = x.first {
      |    bar(y)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock)   = cpg.method.nameExact("foo").block.l
      val List(guardTmpLocal) = methodBlock.astChildren.isLocal.nameNot("self").l
      guardTmpLocal.name shouldBe "<tmp>0"

      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.code shouldBe "guard let _ = x else { return }"

      val List(guardThenBlock) = guardIf.whenTrue.isBlock.l
      val List(ifLetTmpLocal)  = guardThenBlock.astChildren.isLocal.l
      ifLetTmpLocal.name shouldBe "<tmp>1"

      val List(ifLet) = guardThenBlock.astChildren.isControlStructure.l
      ifLet.code should startWith("if let y = x.first")

      val List(ifLetThenBlock) = ifLet.whenTrue.isBlock.l
      val List(yLocal)         = ifLetThenBlock.astChildren.isLocal.l
      yLocal.name shouldBe "y"

      val List(unwrapY, barCall) = ifLetThenBlock.astChildren.isCall.l
      unwrapY.name shouldBe Operators.assignment
      unwrapY.code shouldBe "y = <tmp>1"
      barCall.code shouldBe "bar(y)"
    }

    "testNestedIfLet" in {
      val cpg = code("""
      |func foo(_ a: URL) -> String? {
      |  if let c = Components(url: a), let params = c.items {
      |    if let value = params.first {
      |      return value
      |    }
      |  }
      |  return nil
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("foo").block.l
      methodBlock.astChildren.isLocal.name.l should contain allOf ("<tmp>0", "<tmp>1")

      val List(outerIf) = methodBlock.astChildren.isControlStructure.l
      outerIf.code should startWith("if let c = Components(url: a), let params = c.items")

      val List(condBlock) = outerIf.condition.isBlock.l
      condBlock.ast.isCall.code.l shouldBe List(
        "((<tmp>0 = Components(url: a)) != nil) && ((<tmp>1 = <tmp>0.items) != nil)",
        "(<tmp>0 = Components(url: a)) != nil",
        "<tmp>0 = Components(url: a)",
        "Components(url: a)",
        "(<tmp>1 = <tmp>0.items) != nil",
        "<tmp>1 = <tmp>0.items",
        "<tmp>0.items"
      )

      val List(outerThenBlock)                              = outerIf.whenTrue.isBlock.l
      val List(componentsLocal, paramsLocal, innerTmpLocal) = outerThenBlock.astChildren.isLocal.l
      componentsLocal.name shouldBe "c"
      paramsLocal.name shouldBe "params"
      innerTmpLocal.name shouldBe "<tmp>2"

      val List(unwrapComponents, unwrapParams) = outerThenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapComponents.code shouldBe "c = <tmp>0"
      unwrapParams.code shouldBe "params = <tmp>1"

      val List(innerIf) = outerThenBlock.astChildren.isControlStructure.l
      innerIf.code should startWith("if let value = params.first")

      val List(innerThenBlock) = innerIf.whenTrue.isBlock.l
      val List(valueLocal)     = innerThenBlock.astChildren.isLocal.l
      valueLocal.name shouldBe "value"

      val List(unwrapValue) = innerThenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapValue.code shouldBe "value = <tmp>2"

      val List(returnValue) = innerThenBlock.astChildren.isReturn.l
      returnValue.code shouldBe "return value"
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
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // After desugaring: <tmp>0 in method block
      val List(tmpLocal, iteratorLocal) = methodBlock.local.l
      val tmpName                       = tmpLocal.name
      tmpName shouldBe "<tmp>0"
      iteratorLocal.name shouldBe "iterator"

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: desugared to { (<tmp>0 = iterator.next()) != nil }
      val List(condBlock) = whileNode.condition.isBlock.l

      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($tmpName = iterator.next()) != nil"

      val List(condAssign) = condCheck.argument.assignment.l
      condAssign.code shouldBe s"$tmpName = iterator.next()"
      condAssign.argument(1).code shouldBe tmpName
      condAssign.argument(2).code shouldBe "iterator.next()"

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
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // After desugaring: <tmp>0, <tmp>1 in method block
      val List(tmp0Local, tmp1Local) = methodBlock.local.nameNot("self").l
      val tmp0Name                   = tmp0Local.name
      val tmp1Name                   = tmp1Local.name
      tmp0Name shouldBe "<tmp>0"
      tmp1Name shouldBe "<tmp>1"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { (<tmp>0 = Optional(1)) != nil && (<tmp>1 = Optional(2)) != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"($tmp0Name = Optional(1)) != nil"
      check2.code shouldBe s"($tmp1Name = Optional(2)) != nil"

      val List(assign1, assign2) = andCheck.argument.isCall.argument.assignment.l
      assign1.code shouldBe s"$tmp0Name = Optional(1)"
      assign2.code shouldBe s"$tmp1Name = Optional(2)"

      // Then block: { a = <tmp>0; b = <tmp>1; print(a, b) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aLocal, bLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"
      bLocal.name shouldBe "b"

      val List(unwrapA, unwrapB) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp0Name"
      unwrapB.code shouldBe s"b = $tmp1Name"
    }

    "testIfLetMixed" in {
      val cpg = code("""
      |func test(opt2: Int?) {
      |  if let a = Optional(1), let opt2 {
      |    print(a, opt2)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // After desugaring: <tmp>0 in method block
      val List(tmp0Local) = methodBlock.local.nameNot("self").l
      val tmp0Name        = tmp0Local.name
      tmp0Name shouldBe "<tmp>0"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { (<tmp>0 = Optional(1)) != nil && opt2 != nil }
      val List(condBlock) = ifNode.condition.isBlock.l

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"($tmp0Name = Optional(1)) != nil"
      check2.code shouldBe "opt2 != nil"

      val List(assign1) = check1.argument.assignment.l
      assign1.code shouldBe s"$tmp0Name = Optional(1)"

      // Then block: { a = <tmp>0; print(a, opt2) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"

      val List(unwrapA) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp0Name"
    }

    "testWhileLetMultiple" in {
      val cpg = code("""
      |func test() {
      |  while let a = iterator1.next(), let b = iterator2.next() {
      |    print(a, b)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // After desugaring: <tmp>0, <tmp>1 in method block
      val List(tmp0Local, tmp1Local) = methodBlock.local.nameNot("iterator1", "iterator2").l
      val tmp0Name                   = tmp0Local.name
      val tmp1Name                   = tmp1Local.name
      tmp0Name shouldBe "<tmp>0"
      tmp1Name shouldBe "<tmp>1"

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: { (<tmp>0 = iterator1.next()) != nil && (<tmp>1 = iterator2.next()) != nil }
      val List(condBlock) = whileNode.condition.isBlock.l

      val List(andCheck)       = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(check1, check2) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      check1.code shouldBe s"($tmp0Name = iterator1.next()) != nil"
      check2.code shouldBe s"($tmp1Name = iterator2.next()) != nil"

      val List(assign1, assign2) = andCheck.argument.isCall.argument.assignment.l
      assign1.code shouldBe s"$tmp0Name = iterator1.next()"
      assign2.code shouldBe s"$tmp1Name = iterator2.next()"

      // Loop body: { a = <tmp>0; b = <tmp>1; print(a, b) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      val List(aLocal, bLocal) = bodyBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"
      bLocal.name shouldBe "b"

      val List(unwrapA, unwrapB) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).l
      unwrapA.code shouldBe s"a = $tmp0Name"
      unwrapB.code shouldBe s"b = $tmp1Name"
    }

    "testIfLetMixedWithTuplePattern" in {
      val cpg = code("""
      |func test() {
      |  if let a = foo(), let (b, c) = bar() {
      |    print(a, b, c)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // Two tmp locals: <tmp>0 for foo(), <tmp>1 for bar()
      val List(tmp0Local, tmp1Local) = methodBlock.local.nameNot("self").l
      val tmp0Name                   = tmp0Local.name
      val tmp1Name                   = tmp1Local.name
      tmp0Name shouldBe "<tmp>0"
      tmp1Name shouldBe "<tmp>1"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { (<tmp>0 = foo()) != nil && (<tmp>1 = bar()) != nil }
      val List(condBlock) = ifNode.condition.isBlock.l
      val List(andCheck)  = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      andCheck.argument(1).code shouldBe s"($tmp0Name = foo()) != nil"
      andCheck.argument(2).code shouldBe s"($tmp1Name = bar()) != nil"

      // Then block: { a = <tmp>0; b = <tmp>1.0; c = <tmp>1.1; print(a, b, c) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.nameExact("a").l
      aLocal.name shouldBe "a"

      val List(unwrapA) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmp0Name").l
      unwrapA.argument(1).code shouldBe "a"
      unwrapA.argument(2).code shouldBe tmp0Name

      val List(bAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmp1Name.0").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe s"$tmp1Name.0"

      val List(cAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"c = $tmp1Name.1").l
      cAssign.argument(1).code shouldBe "c"
      cAssign.argument(2).code shouldBe s"$tmp1Name.1"
    }

    "testWhileLetMixedWithTuplePattern" in {
      val cpg = code("""
      |func test() {
      |  while let a = foo(), let (b, c) = bar() {
      |    print(a, b, c)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock)          = cpg.method.nameExact("test").block.l
      val List(tmp0Local, tmp1Local) = methodBlock.local.nameNot("self").l
      val tmp0Name                   = tmp0Local.name
      val tmp1Name                   = tmp1Local.name
      tmp0Name shouldBe "<tmp>0"
      tmp1Name shouldBe "<tmp>1"

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: { (<tmp>0 = foo()) != nil && (<tmp>1 = bar()) != nil }
      val List(condBlock) = whileNode.condition.isBlock.l
      val List(andCheck)  = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      andCheck.argument(1).code shouldBe s"($tmp0Name = foo()) != nil"
      andCheck.argument(2).code shouldBe s"($tmp1Name = bar()) != nil"

      // Body: { a = <tmp>0; b = <tmp>1.0; c = <tmp>1.1; print(a, b, c) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      val List(aLocal) = bodyBlock.astChildren.isLocal.nameExact("a").l
      aLocal.name shouldBe "a"

      val List(unwrapA) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmp0Name").l
      unwrapA.argument(1).code shouldBe "a"
      unwrapA.argument(2).code shouldBe tmp0Name

      val List(bAssign) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmp1Name.0").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe s"$tmp1Name.0"

      val List(cAssign) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"c = $tmp1Name.1").l
      cAssign.argument(1).code shouldBe "c"
      cAssign.argument(2).code shouldBe s"$tmp1Name.1"
    }

    "testIfLetPureTuple" in {
      val cpg = code("""
      |func test() {
      |  if let (a, b) = foo() {
      |    print(a, b)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // tmp variable in method block for nil check
      val List(tmpLocal) = methodBlock.local.nameNot("self").l
      val tmpName        = tmpLocal.name
      tmpName shouldBe "<tmp>0"

      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { (<tmp>0 = foo()) != nil }
      val List(condBlock) = ifNode.condition.isBlock.l
      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($tmpName = foo()) != nil"

      val List(condAssign) = condCheck.argument.assignment.l
      condAssign.code shouldBe s"$tmpName = foo()"

      // Then block: { a = <tmp>0.0; b = <tmp>0.1; print(a, b) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(aAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      aAssign.argument(1).code shouldBe "a"
      aAssign.argument(2).code shouldBe s"$tmpName.0"

      val List(bAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe s"$tmpName.1"

      val List(printCall) = thenBlock.astChildren.isCall.nameExact("print").l
      printCall.code shouldBe "print(a, b)"
    }

    "testWhileLetPureTuple" in {
      val cpg = code("""
      |func test() {
      |  while let (a, b) = foo() {
      |    print(a, b)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      val List(tmpLocal)    = methodBlock.local.nameNot("self").l
      val tmpName           = tmpLocal.name
      tmpName shouldBe "<tmp>0"

      val List(whileNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l

      // Condition: { (<tmp>0 = foo()) != nil }
      val List(condBlock) = whileNode.condition.isBlock.l
      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($tmpName = foo()) != nil"

      // Body: { a = <tmp>0.0; b = <tmp>0.1; print(a, b) }
      val List(bodyBlock) = whileNode.whenTrue.isBlock.l

      val List(aAssign) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"a = $tmpName.0").l
      aAssign.argument(1).code shouldBe "a"
      aAssign.argument(2).code shouldBe s"$tmpName.0"

      val List(bAssign) = bodyBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact(s"b = $tmpName.1").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe s"$tmpName.1"

      val List(printCall) = bodyBlock.astChildren.isCall.nameExact("print").l
      printCall.code shouldBe "print(a, b)"
    }

    "testIfLetTupleWithDependentBinding" in {
      val cpg = code("""
      |func test() {
      |  if let (a, b) = foo(), let c = a.bar() {
      |    print(a, b, c)
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l

      // <tmp>0 is allocated for a.bar(), <tmp>1 for the tuple binding foo()
      val List(cTmpLocal, tupleTmpLocal) = methodBlock.local.nameNot("self", "a", "b", "c").l
      cTmpLocal.name shouldBe "<tmp>0"
      tupleTmpLocal.name shouldBe "<tmp>1"

      val List(ifNode)    = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      val List(condBlock) = ifNode.condition.isBlock.l

      // Condition: { (<tmp>0 = a.bar()) != nil && (<tmp>1 = foo()) != nil }
      // where a.bar() structurally uses <tmp>1.0 as the receiver
      val List(andCheck) = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      andCheck.argument(1).code shouldBe "(<tmp>0 = a.bar()) != nil"
      andCheck.argument(2).code shouldBe "(<tmp>1 = foo()) != nil"

      // The receiver of bar() in the condition is <tmp>1.0 (tuple field access), not a plain 'a'
      val List(barCall) = condBlock.ast.isCall.nameExact("bar").l
      barCall.code shouldBe "a.bar()"
      val List(barReceiver) = barCall.receiver.isCall.nameExact(Operators.fieldAccess).l
      barReceiver.code shouldBe "<tmp>1.0"
      barReceiver.argumentIndex shouldBe 0
      barCall.argument(0).code shouldBe "<tmp>1.0"

      // Then block: { c = <tmp>0; a = <tmp>1.0; b = <tmp>1.1; print(a, b, c) }
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      thenBlock.astChildren.isLocal.name.sorted shouldBe Seq("a", "b", "c")

      val List(cAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("c = <tmp>0").l
      cAssign.argument(1).code shouldBe "c"
      cAssign.argument(2).code shouldBe "<tmp>0"

      val List(aAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>1.0").l
      aAssign.argument(1).code shouldBe "a"
      aAssign.argument(2).code shouldBe "<tmp>1.0"

      val List(bAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("b = <tmp>1.1").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe "<tmp>1.1"

      val List(printCall) = thenBlock.astChildren.isCall.nameExact("print").l
      printCall.code shouldBe "print(a, b, c)"
    }

    "testIfLetNestedWithTuple" in {
      val cpg = code("""
      |func test() {
      |  if let (a, b) = foo() {
      |    if let c = bar(a) {
      |      print(a, b, c)
      |    }
      |  }
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l

      // Outer method block: <tmp>0 for foo() tuple binding
      val List(outerTmpLocal) = methodBlock.local.nameNot("self").l
      outerTmpLocal.name shouldBe "<tmp>0"

      val List(outerIf) = methodBlock.astChildren.isControlStructure.controlStructureType(ControlStructureTypes.IF).l
      val List(outerCondBlock) = outerIf.condition.isBlock.l

      // Outer condition: { (<tmp>0 = foo()) != nil }
      val List(outerCondCheck) = outerCondBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      outerCondCheck.code shouldBe "(<tmp>0 = foo()) != nil"

      // Outer then block: a = <tmp>0.0; b = <tmp>0.1; then inner if, plus <tmp>1 local for the inner binding
      val List(outerThenBlock) = outerIf.whenTrue.isBlock.l

      outerThenBlock.astChildren.isLocal.name.sorted shouldBe Seq("<tmp>1", "a", "b")

      val List(aAssign) = outerThenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>0.0").l
      aAssign.argument(1).code shouldBe "a"
      aAssign.argument(2).code shouldBe "<tmp>0.0"

      val List(bAssign) = outerThenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("b = <tmp>0.1").l
      bAssign.argument(1).code shouldBe "b"
      bAssign.argument(2).code shouldBe "<tmp>0.1"

      // Inner if: bar(a) uses 'a' as a plain identifier (not a field access)
      // because by this point 'a' is already declared as a local in the outer then-block
      val List(innerIf) = outerThenBlock.astChildren.isControlStructure.controlStructureType(ControlStructureTypes.IF).l
      val List(innerCondBlock) = innerIf.condition.isBlock.l
      val List(innerCondCheck) = innerCondBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      innerCondCheck.code shouldBe "(<tmp>1 = bar(a)) != nil"

      // 'a' in bar(a) is a plain identifier reference, not a field access
      val List(barCall) = innerCondBlock.ast.isCall.nameExact("bar").l
      barCall.code shouldBe "bar(a)"
      val List(aArg) = barCall.argument.isIdentifier.nameNot("self").l
      aArg.code shouldBe "a"

      // Inner then block: { c = <tmp>1; print(a, b, c) }
      val List(innerThenBlock) = innerIf.whenTrue.isBlock.l

      innerThenBlock.astChildren.isLocal.name.loneElement shouldBe "c"

      val List(cAssign) = innerThenBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("c = <tmp>1").l
      cAssign.argument(1).code shouldBe "c"
      cAssign.argument(2).code shouldBe "<tmp>1"

      val List(printCall) = innerThenBlock.astChildren.isCall.nameExact("print").l
      printCall.code shouldBe "print(a, b, c)"
    }

    "testGuardLetNestedInIfLetBody" in {
      val cpg = code("""
      |func test(optionalValue: Int?) {
      |  if let sql = optionalValue {
      |    guard let user = bar() else {
      |      return
      |    }
      |    print(user)
      |  }
      |}
      |""".stripMargin)
      val List(ifNode)    = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).code(".*if let sql.*").l
      val List(thenBlock) = ifNode.whenTrue.isBlock.l

      val List(sqlLocal, guardTmpLocal) = thenBlock.astChildren.isLocal.l
      sqlLocal.name shouldBe "sql"
      val guardTmpName = guardTmpLocal.name
      guardTmpName shouldBe "<tmp>1"

      val List(guardIf) = thenBlock.astChildren.isControlStructure.controlStructureType(ControlStructureTypes.IF).l
      guardIf.code should startWith("guard let user = bar()")

      val List(condBlock) = guardIf.condition.isBlock.l
      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"($guardTmpName = bar()) != nil"

      val List(condAssign) = condCheck.argument.isCall.nameExact(Operators.assignment).l
      condAssign.code shouldBe s"$guardTmpName = bar()"
      condAssign.argument(1).code shouldBe guardTmpName
      condAssign.argument(2).code shouldBe "bar()"

      val List(condNil) = condCheck.argument.isLiteral.l
      condNil.code shouldBe "nil"

      val List(guardThenBlock) = guardIf.whenTrue.isBlock.l
      val List(userLocal)      = guardThenBlock.astChildren.isLocal.l
      userLocal.name shouldBe "user"

      val List(unwrapUser, printCall) = guardThenBlock.astChildren.isCall.l
      unwrapUser.name shouldBe Operators.assignment
      unwrapUser.code shouldBe s"user = $guardTmpName"
      unwrapUser.argument(1).code shouldBe "user"
      unwrapUser.argument(2).code shouldBe guardTmpName
      printCall.name shouldBe "print"
      printCall.code shouldBe "print(user)"

      val List(elseReturn) = guardIf.whenFalse.isReturn.l
      elseReturn.code shouldBe "return"
    }

  }

}
