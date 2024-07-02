// This test file has been translated from swift/test/Parse/availability_query.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class AvailabilityQueryTests extends AstSwiftSrc2CpgSuite {

  "AvailabilityQueryTests" should {

    "testAvailabilityQuery1" in {
      val cpg                      = code("if #available(OSX 10.51, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(OSX 10.51, *)"
      val List(arg1, arg2) = condition.argument.isLiteral.l
      arg1.code shouldBe "OSX 10.51"
      arg1.argumentIndex shouldBe 1
      arg2.code shouldBe "*"
      arg2.argumentIndex shouldBe 2
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery2" in {
      val cpg                      = code("if #unavailable(OSX 10.51, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#unavailable(OSX 10.51, *)"
      val List(arg1, arg2) = condition.argument.isLiteral.l
      arg1.code shouldBe "OSX 10.51"
      arg1.argumentIndex shouldBe 1
      arg2.code shouldBe "*"
      arg2.argumentIndex shouldBe 2
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery5b" in {
      val cpg                      = code("if let _ = Optional(5), #unavailable(OSX 10.52, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(assignment, unavailable) = ifControlStructure.condition.astChildren.isCall.l
      assignment.code shouldBe "let _ = Optional(5)"
      assignment.name shouldBe Operators.assignment
      unavailable.code shouldBe "#unavailable(OSX 10.52, *)"
      unavailable.name shouldBe "#unavailable"
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery6" in {
      val cpg                      = code("if #available(OSX 10.51, *), #available(OSX 10.52, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(available1, available2) = ifControlStructure.condition.astChildren.isCall.l
      available1.code shouldBe "#available(OSX 10.51, *)"
      available1.name shouldBe "#available"
      available2.code shouldBe "#available(OSX 10.52, *)"
      available2.name shouldBe "#available"
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery10" in {
      val cpg                      = code("if #available(OSX) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(OSX)"
      val List(arg1) = condition.argument.isLiteral.l
      arg1.code shouldBe "OSX"
      arg1.argumentIndex shouldBe 1
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery12" in {
      val cpg                      = code("if #available(OSX 10.51) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(OSX 10.51)"
      val List(arg1) = condition.argument.isLiteral.l
      arg1.code shouldBe "OSX 10.51"
      arg1.argumentIndex shouldBe 1
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery13" in {
      val cpg                      = code("if #available(iDishwasherOS 10.51) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(iDishwasherOS 10.51)"
      val List(arg1) = condition.argument.isLiteral.l
      arg1.code shouldBe "iDishwasherOS 10.51"
      arg1.argumentIndex shouldBe 1
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery14" in {
      val cpg                      = code(" if #available(iDishwasherOS 10.51, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(iDishwasherOS 10.51, *)"
      val List(arg1, arg2) = condition.argument.isLiteral.l
      arg1.code shouldBe "iDishwasherOS 10.51"
      arg1.argumentIndex shouldBe 1
      arg2.code shouldBe "*"
      arg2.argumentIndex shouldBe 2
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery19" in {
      val cpg                      = code("if #available(OSX 10.51, OSX 10.52, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(OSX 10.51, OSX 10.52, *)"
      val List(arg1, arg2, arg3) = condition.argument.isLiteral.l
      arg1.code shouldBe "OSX 10.51"
      arg1.argumentIndex shouldBe 1
      arg2.code shouldBe "OSX 10.52"
      arg2.argumentIndex shouldBe 2
      arg3.code shouldBe "*"
      arg3.argumentIndex shouldBe 3
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery24" in {
      val cpg                      = code("if #available(*) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(condition) = ifControlStructure.condition.isCall.l
      condition.code shouldBe "#available(*)"
      val List(arg1) = condition.argument.isLiteral.l
      arg1.code shouldBe "*"
      arg1.argumentIndex shouldBe 1
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery35" in {
      val cpg                      = code("if 1 != 2, #available(iOS 8.0, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(notEquals, available) = ifControlStructure.condition.astChildren.isCall.l
      notEquals.code shouldBe "1 != 2"
      notEquals.name shouldBe Operators.notEquals
      available.code shouldBe "#available(iOS 8.0, *)"
      available.name shouldBe "#available"
      ifControlStructure.whenFalse shouldBe empty
    }

    "testAvailabilityQuery36" in {
      val cpg                      = code("if case 42 = 42, #available(iOS 8.0, *) {}")
      val List(ifControlStructure) = cpg.controlStructure.isIf.l
      ifControlStructure.whenTrue shouldBe empty
      val List(notEquals, available) = ifControlStructure.condition.astChildren.isCall.l
      notEquals.code shouldBe "case 42 = 42"
      notEquals.name shouldBe Operators.equals
      available.code shouldBe "#available(iOS 8.0, *)"
      available.name shouldBe "#available"
      ifControlStructure.whenFalse shouldBe empty
    }

  }

}
