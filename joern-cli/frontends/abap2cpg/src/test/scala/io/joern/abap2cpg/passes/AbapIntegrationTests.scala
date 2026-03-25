package io.joern.abap2cpg.passes

import io.joern.abap2cpg.testfixtures.Abap2CpgSuite
import io.shiftleft.semanticcpg.language.*

/** Integration tests that run the full Abap2Cpg pipeline (requires abapgen binary).
  *
  * Test code lives in tests/code/abap/simple.abap.
  */
class AbapIntegrationTests extends Abap2CpgSuite {

  "Full pipeline for a simple ABAP class" should {

    val abapCode =
      """CLASS zcl_simple DEFINITION PUBLIC.
        |  PUBLIC SECTION.
        |    METHODS greet
        |      IMPORTING
        |        iv_name TYPE string
        |      RETURNING
        |        VALUE(rv_result) TYPE string.
        |    METHODS add
        |      IMPORTING
        |        iv_a TYPE i
        |        iv_b TYPE i
        |      RETURNING
        |        VALUE(rv_sum) TYPE i.
        |ENDCLASS.
        |CLASS zcl_simple IMPLEMENTATION.
        |  METHOD greet.
        |    DATA: lv_greeting TYPE string.
        |    lv_greeting = iv_name.
        |    rv_result = lv_greeting.
        |  ENDMETHOD.
        |  METHOD add.
        |    rv_sum = iv_a.
        |  ENDMETHOD.
        |ENDCLASS.
        |""".stripMargin

    // abapgen requires ABAP-standard file naming: <name>.clas.abap for class files
    val fileName = "zcl_simple.clas.abap"

    "create a type declaration for the class" in {
      val cpg = code(abapCode, fileName)
      cpg.typeDecl.nameExact("zcl_simple").size shouldBe 1
    }

    "create methods with class-qualified full names" in {
      val cpg = code(abapCode, fileName)
      cpg.method.fullName.l should contain allOf (
        "zcl_simple::greet",
        "zcl_simple::add"
      )
    }

    "create accessible importing parameters" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      withClue(s"All method full names: ${cpg.method.fullName.l}\nAll params in CPG: ${cpg.parameter.name.l}\ngreet params: ${greet.parameter.name.l}") {
        greet.parameter.name.l should contain("iv_name")
      }
    }

    "create accessible locals" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      greet.local.name.l should contain("lv_greeting")
    }

    "create REF edges from identifiers to parameters" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      val param = greet.parameter.nameExact("iv_name").head
      greet.ast.isIdentifier.nameExact("iv_name").flatMap(_._refOut).headOption shouldBe Some(param)
    }

    "create REF edges from identifiers to locals" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      val local = greet.local.nameExact("lv_greeting").head
      greet.ast.isIdentifier.nameExact("lv_greeting").flatMap(_._refOut).headOption shouldBe Some(local)
    }

    "create an assignment call for lv_greeting = iv_name" in {
      val cpg = code(abapCode, fileName)
      cpg.method
        .fullNameExact("zcl_simple::greet")
        .ast
        .isCall
        .nameExact("<operator>.assignment")
        .size should be >= 1
    }
  }
}
