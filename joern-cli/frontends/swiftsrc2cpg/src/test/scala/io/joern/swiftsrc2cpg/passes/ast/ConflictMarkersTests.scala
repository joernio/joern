// This test file has been translated from swift/test/Parse/conflict_markers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ConflictMarkersTests extends AstSwiftSrc2CpgSuite {

  "ConflictMarkersTests" should {

    "testConflictMarkers2" in {
      val cpg = code("""
        |prefix operator <<<<<<<
        |infix operator <<<<<<<
        |""".stripMargin)
      // operator decls cannot be expressed within the CPG and are of no value for us
      cpg.literal.codeExact("operator") shouldBe empty
    }

    "testConflictMarkers3" in {
      val cpg = code("""
        |prefix func <<<<<<< (x : String) {}
        |func <<<<<<< (x : String, y : String) {}
        |""".stripMargin)
      val List(f1, f2) = cpg.method.nameExact("<<<<<<<").l
      f1.fullName shouldBe "Test0.swift:<global>:<<<<<<<:ANY(String)"
      f1.signature shouldBe "ANY(String)"
      f2.fullName shouldBe "Test0.swift:<global>:<<<<<<<:ANY(String,String)"
      f2.signature shouldBe "ANY(String,String)"
      val List(x1) = f1.parameter.l
      x1.name shouldBe "x"
      x1.typeFullName shouldBe "String"
      val List(x2, y) = f2.parameter.l
      x2.name shouldBe "x"
      x2.typeFullName shouldBe "String"
      y.name shouldBe "y"
      y.typeFullName shouldBe "String"
    }

    "testConflictMarkers5" in {
      val cpg = code("""
        |prefix func >>>>>>> (x : String) {}
        |func >>>>>>> (x : String, y : String) {}
        |""".stripMargin)
      val List(f1, f2) = cpg.method.nameExact(">>>>>>>").l
      f1.fullName shouldBe "Test0.swift:<global>:>>>>>>>:ANY(String)"
      f1.signature shouldBe "ANY(String)"
      f2.fullName shouldBe "Test0.swift:<global>:>>>>>>>:ANY(String,String)"
      f2.signature shouldBe "ANY(String,String)"
      val List(x1) = f1.parameter.l
      x1.name shouldBe "x"
      x1.typeFullName shouldBe "String"
      val List(x2, y) = f2.parameter.l
      x2.name shouldBe "x"
      x2.typeFullName shouldBe "String"
      y.name shouldBe "y"
      y.typeFullName shouldBe "String"
    }

    "testConflictMarkers9" in {
      val cpg = code("""
        |<<<<<<<"HEAD:fake_conflict_markers.swift" // No error
        |>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift" // No error
        |""".stripMargin)
      val List(call1) = cpg.call.nameExact("<<<<<<<").l
      call1.code shouldBe """<<<<<<<"HEAD:fake_conflict_markers.swift""""
      call1.argument.code.l shouldBe List(""""HEAD:fake_conflict_markers.swift"""")
      val List(call2) = cpg.call.nameExact(">>>>>>>").l
      call2.code shouldBe """>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift""""
      call2.argument.code.l shouldBe List(""""18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift"""")
    }

  }

}
