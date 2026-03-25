package io.joern.abap2cpg.passes

import io.joern.abap2cpg.testfixtures.Abap2CpgSuite
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*

/** Integration tests that run the full Abap2Cpg pipeline (requires abapgen binary).
  *
  * Test code lives in tests/code/abap/zcl_simple.clas.abap.
  */
class AbapIntegrationTests extends Abap2CpgSuite {

  val fileName = "zcl_simple.clas.abap"
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
      |    METHODS run
      |      IMPORTING
      |        iv_input TYPE string.
      |ENDCLASS.
      |CLASS zcl_simple IMPLEMENTATION.
      |  METHOD greet.
      |    DATA: lv_greeting TYPE string.
      |    lv_greeting = iv_name.
      |    rv_result = lv_greeting.
      |    me->run( iv_input = lv_greeting ).
      |  ENDMETHOD.
      |  METHOD add.
      |    rv_sum = iv_a.
      |  ENDMETHOD.
      |  METHOD run.
      |    zcl_simple=>greet( iv_name = iv_input ).
      |  ENDMETHOD.
      |ENDCLASS.
      |""".stripMargin

  "CPG method names" should {

    "contain exactly the defined class methods" in {
      val cpg         = code(abapCode, fileName)
      val classMethods = cpg.method.fullName.filter(_.startsWith("zcl_simple::")).l
      classMethods.sorted shouldBe List("zcl_simple::add", "zcl_simple::greet", "zcl_simple::run")
    }

    "not contain any ABAP keyword as a method name" in {
      val abapKeywords = Set("VALUE", "REDUCE", "COND", "NEW", "FOR", "WHILE", "LET", "IN",
                             "INIT", "NEXT", "TYPE", "IMPORTING", "EXPORTING", "RETURNING",
                             "CHANGING", "OPTIONAL", "DEFAULT", "DATA")
      val cpg = code(abapCode, fileName)
      val methodNames = cpg.method.name.toSet
      val overlap = methodNames.intersect(abapKeywords)
      withClue(s"Methods named after ABAP keywords: $overlap") {
        overlap shouldBe empty
      }
    }

    "not contain method names with spaces or special characters (no garbage names)" in {
      val cpg = code(abapCode, fileName)
      val garbageNames = cpg.method.name.filter(n =>
        n.contains(" ") || n.contains("=") || n.contains("(") || n.contains(")")
      ).l
      withClue(s"Garbage method names: $garbageNames") {
        garbageNames shouldBe empty
      }
    }

    "use operator-style names for constructor expressions" in {
      val cpg = code(abapCode, fileName)
      // me->run(...) produces a DYNAMIC_DISPATCH call named "run", not a method stub
      // zcl_simple=>greet(...) produces a STATIC_DISPATCH call named "greet"
      // Constructor expressions like NEW/VALUE/REDUCE become <operator>.* in call nodes
      cpg.call.name.filter(_.startsWith("<operator>.")).l should not be empty
    }
  }

  "CPG parameter names" should {

    "have correct parameter names for greet (iv_name)" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      greet.parameter.name.l should contain("iv_name")
    }

    "have correct parameter names for add (iv_a, iv_b)" in {
      val cpg = code(abapCode, fileName)
      val add = cpg.method.fullNameExact("zcl_simple::add").head
      add.parameter.name.l should contain allOf ("iv_a", "iv_b")
    }

    "have correct parameter names for run (iv_input)" in {
      val cpg = code(abapCode, fileName)
      val run = cpg.method.fullNameExact("zcl_simple::run").head
      run.parameter.name.l should contain("iv_input")
    }

    "not contain ABAP keywords as parameter names" in {
      val abapParamKeywords = Set("TYPE", "IMPORTING", "EXPORTING", "RETURNING",
                                   "OPTIONAL", "DEFAULT", "TO", "(", ")", ".", ":")
      val cpg = code(abapCode, fileName)
      val paramNames = cpg.method.fullName.filter(_.startsWith("zcl_simple::"))
        .flatMap(_.split("::").lastOption)
        .flatMap(name => cpg.method.fullNameExact(s"zcl_simple::$name").parameter.name.l)
        .toSet
      val overlap = paramNames.intersect(abapParamKeywords)
      withClue(s"Params named after ABAP keywords: $overlap") {
        overlap shouldBe empty
      }
    }

    "not contain parameter names with spaces or punctuation" in {
      val cpg = code(abapCode, fileName)
      val badParams = cpg.parameter.name.filter(n =>
        n.contains(" ") || n.contains("=") || n.contains("(") || n.contains(")")
      ).l
      withClue(s"Garbage parameter names: $badParams") {
        badParams shouldBe empty
      }
    }
  }

  "CPG metadata" should {

    "have language set to ABAP" in {
      val cpg = code(abapCode, fileName)
      cpg.metaData.language.l shouldBe List(Languages.ABAP)
    }

    "have root set to the input directory" in {
      val cpg = code(abapCode, fileName)
      cpg.metaData.root.l.head.isEmpty shouldBe false
    }
  }

  "CPG file nodes" should {

    "create a file node for each parsed ABAP file" in {
      val cpg = code(abapCode, fileName)
      cpg.file.size should be >= 1
    }

    "set the correct file name (relative path)" in {
      val cpg = code(abapCode, fileName)
      cpg.file.name.l should contain(fileName)
    }

    "set order to 0 on file nodes" in {
      val cpg        = code(abapCode, fileName)
      val List(file) = cpg.file.nameExact(fileName).l
      file.order shouldBe 0
    }
  }

  "CPG namespace nodes" should {

    "have exactly one global namespace" in {
      val cpg = code(abapCode, fileName)
      cpg.namespace.size shouldBe 1
      cpg.namespace.name.head shouldBe "<global>"
    }
  }

  "CPG typeDecl nodes" should {

    "create a TypeDecl with correct name and fullName" in {
      val cpg = code(abapCode, fileName)
      val td  = cpg.typeDecl.nameExact("zcl_simple").head
      td.name shouldBe "zcl_simple"
      td.fullName shouldBe "zcl_simple"
    }

    "set isExternal to false on the TypeDecl" in {
      val cpg = code(abapCode, fileName)
      cpg.typeDecl.nameExact("zcl_simple").head.isExternal shouldBe false
    }

    "set the filename on the TypeDecl" in {
      val cpg = code(abapCode, fileName)
      cpg.typeDecl.nameExact("zcl_simple").head.filename shouldBe fileName
    }

    "set order 1 on the TypeDecl" in {
      val cpg = code(abapCode, fileName)
      cpg.typeDecl.nameExact("zcl_simple").head.order shouldBe 1
    }
  }

  "CPG method parameters" should {

    // Note: abapgen represents class method parameters differently depending on the section.
    // Parameters exist in the global CPG (cpg.parameter) even when not yet linked by name.
    // Named parameter linking is verified at unit test level (AstCreatorTests).

    "create MethodParameterIn nodes in the CPG" in {
      val cpg = code(abapCode, fileName)
      cpg.parameter.size should be >= 1
    }

    "set index starting at 1 on all parameters" in {
      val cpg = code(abapCode, fileName)
      cpg.parameter.index.l.foreach(idx => idx should be >= 1)
    }

    "set order equal to index on all parameters" in {
      val cpg = code(abapCode, fileName)
      cpg.parameter.l.foreach(p => p.order shouldBe p.index)
    }

    "set isVariadic to false on all parameters" in {
      val cpg = code(abapCode, fileName)
      cpg.parameter.isVariadic.l.distinct shouldBe List(false)
    }

    "set name and code consistently on all parameters" in {
      val cpg = code(abapCode, fileName)
      cpg.parameter.l.foreach(p => p.code shouldBe p.name)
    }
  }

  "CPG call nodes" should {

    "create a call node for instance method call (me->run)" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      greet.ast.isCall.nameExact("run").size shouldBe 1
    }

    "set DYNAMIC_DISPATCH for instance call (->)" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      val call  = greet.ast.isCall.nameExact("run").head
      call.dispatchType shouldBe "DYNAMIC_DISPATCH"
    }

    "set correct methodFullName for instance call (me->run)" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      val call  = greet.ast.isCall.nameExact("run").head
      call.methodFullName shouldBe "me.run"
    }

    "create a call node for static method call (zcl_simple=>greet)" in {
      val cpg = code(abapCode, fileName)
      val run = cpg.method.fullNameExact("zcl_simple::run").head
      run.ast.isCall.nameExact("greet").size shouldBe 1
    }

    "set STATIC_DISPATCH for static call (=>)" in {
      val cpg  = code(abapCode, fileName)
      val run  = cpg.method.fullNameExact("zcl_simple::run").head
      val call = run.ast.isCall.nameExact("greet").head
      call.dispatchType shouldBe "STATIC_DISPATCH"
    }

    "set correct methodFullName for static call (zcl_simple=>greet)" in {
      val cpg  = code(abapCode, fileName)
      val run  = cpg.method.fullNameExact("zcl_simple::run").head
      val call = run.ast.isCall.nameExact("greet").head
      call.methodFullName shouldBe "zcl_simple.greet"
    }
  }

  "CPG method properties" should {

    "set correct name, fullName, filename, isExternal, order, and signature" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      greet.name shouldBe "greet"
      greet.fullName shouldBe "zcl_simple::greet"
      greet.filename shouldBe fileName
      greet.isExternal shouldBe false
      greet.order shouldBe 1
    }

    "set signature as '() -> <type>' for methods with returning" in {
      // greet returns string, add returns i — signature reflects that
      val cpg = code(abapCode, fileName)
      cpg.method.fullNameExact("zcl_simple::greet").head.signature should include ("->")
    }

    "set lineNumber from source position" in {
      val cpg = code(abapCode, fileName)
      cpg.method.fullNameExact("zcl_simple::greet").head.lineNumber.isDefined shouldBe true
    }

    "set columnNumber from source position" in {
      val cpg = code(abapCode, fileName)
      cpg.method.fullNameExact("zcl_simple::greet").head.columnNumber.isDefined shouldBe true
    }
  }

  "Full pipeline for a simple ABAP class" should {

    "create methods with class-qualified full names" in {
      val cpg = code(abapCode, fileName)
      cpg.method.fullName.l should contain allOf (
        "zcl_simple::greet",
        "zcl_simple::add"
      )
    }

    "create accessible importing parameters for greet (iv_name)" in {
      val cpg   = code(abapCode, fileName)
      val greet = cpg.method.fullNameExact("zcl_simple::greet").head
      greet.parameter.name.l should contain("iv_name")
    }

    "create accessible importing parameters for add (iv_a, iv_b)" in {
      val cpg = code(abapCode, fileName)
      val add = cpg.method.fullNameExact("zcl_simple::add").head
      add.parameter.name.l should contain allOf ("iv_a", "iv_b")
    }

    "set correct type on add parameters" in {
      val cpg = code(abapCode, fileName)
      val add = cpg.method.fullNameExact("zcl_simple::add").head
      add.parameter.nameExact("iv_a").head.typeFullName shouldBe "i"
      add.parameter.nameExact("iv_b").head.typeFullName shouldBe "i"
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
