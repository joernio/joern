package io.joern.abap2cpg.passes

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.testfixtures.AbapCpgFixture
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language.*

class AstCreatorTests extends AbapCpgFixture {

  "AstCreator method and parameter name correctness" should {

    "produce exact method names from the source" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").size shouldBe 1
    }

    "produce exact parameter names from the source" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD",
          importing = Seq(Parameter("IV_NAME", "string"), Parameter("IV_FLAG", "abap_bool"))
        )
      )
      val paramNames = cpg.method.nameExact("MY_METHOD").parameter.name.l
      paramNames should contain allOf ("IV_NAME", "IV_FLAG")
      paramNames should not contain "TYPE"
      paramNames should not contain "IMPORTING"
      paramNames should not contain "string"
    }

    "not produce method names containing spaces" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.method.name.filter(_.contains(" ")).l shouldBe empty
    }

    "not produce parameter names containing spaces or punctuation" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_X", "i")))
      )
      cpg.parameter.name.filter(n => n.contains(" ") || n.contains("(") || n.contains(")")).l shouldBe empty
    }

    "produce class-qualified fullName for class methods" in {
      val cpg = cpgForProgram(programWithClass("ZCL_SIMPLE", "GREET"))
      cpg.method.nameExact("GREET").head.fullName shouldBe "ZCL_SIMPLE::GREET"
    }
  }

  "AstCreator file and namespace nodes" should {

    "create a file node with the program file name" in {
      val program = programWithMethod("MY_METHOD").copy(fileName = "my_program.abap")
      val cpg     = cpgForProgram(program)
      cpg.file.nameExact("my_program.abap").size shouldBe 1
    }

    "set order 0 on the file node" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.file.head.order shouldBe 0
    }

    "create a namespace block linked to the file" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.namespaceBlock.size shouldBe 1
    }

    "create a global namespace node" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.namespace.size shouldBe 1
      cpg.namespace.name.head shouldBe "<global>"
    }
  }

  "AstCreator for type declarations" should {

    "create a TypeDecl for a class" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.typeDecl.nameExact("MY_CLASS").size shouldBe 1
    }

    "set the correct fullName on TypeDecl" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      val td  = cpg.typeDecl.nameExact("MY_CLASS").head
      td.fullName shouldBe "MY_CLASS"
    }

    "set isExternal to false on TypeDecl" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.typeDecl.nameExact("MY_CLASS").head.isExternal shouldBe false
    }

    "set the filename on TypeDecl" in {
      val program = programWithClass("MY_CLASS", "MY_METHOD").copy(fileName = "my_class.clas.abap")
      val cpg     = cpgForProgram(program)
      cpg.typeDecl.nameExact("MY_CLASS").head.filename shouldBe "my_class.clas.abap"
    }

    "set order 1 on the first TypeDecl" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.typeDecl.nameExact("MY_CLASS").head.order shouldBe 1
    }
  }

  "AstCreator method properties" should {

    "set correct name" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.name shouldBe "MY_METHOD"
    }

    "set correct fullName for standalone method" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.fullName shouldBe "MY_METHOD"
    }

    "set correct fullName for class method (CLASS::METHOD)" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.fullName shouldBe "MY_CLASS::MY_METHOD"
    }

    "set filename on method" in {
      val program = programWithMethod("MY_METHOD").copy(fileName = "my_program.abap")
      val cpg     = cpgForProgram(program)
      cpg.method.nameExact("MY_METHOD").head.filename shouldBe "my_program.abap"
    }

    "set isExternal to false" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.isExternal shouldBe false
    }

    "set order 1 for first method" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.order shouldBe 1
    }

    "set signature with no params as '() -> void'" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").head.signature shouldBe "() -> void"
    }

    "set signature with importing params" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_X", "string"), Parameter("IV_Y", "int")))
      )
      cpg.method.nameExact("MY_METHOD").head.signature shouldBe "(string, int) -> void"
    }

    "set signature with returning param" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string")))
      )
      cpg.method.nameExact("MY_METHOD").head.signature shouldBe "() -> string"
    }

    "set lineNumber when span has position" in {
      val span    = TextSpan(start = Some(Position(row = 6, col = 5)))
      val program = programWithMethod("MY_METHOD").copy(methods = Seq(
        MethodDef("MY_METHOD", None, false, MethodParameters(), span = span)
      ))
      val cpg = cpgForProgram(program)
      cpg.method.nameExact("MY_METHOD").head.lineNumber shouldBe Some(6)
    }

    "set columnNumber when span has position" in {
      val span    = TextSpan(start = Some(Position(row = 6, col = 5)))
      val program = programWithMethod("MY_METHOD").copy(methods = Seq(
        MethodDef("MY_METHOD", None, false, MethodParameters(), span = span)
      ))
      val cpg = cpgForProgram(program)
      cpg.method.nameExact("MY_METHOD").head.columnNumber shouldBe Some(5)
    }
  }

  "AstCreator parameter properties" should {

    "set correct name and code on parameter" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string"))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.head
      param.name shouldBe "IV_INPUT"
      param.code shouldBe "IV_INPUT"
    }

    "set correct typeFullName on parameter" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string"))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.typeFullName shouldBe "string"
    }

    "set index starting at 1 for first parameter" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string"))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.index shouldBe 1
    }

    "set order equal to index" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string"))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.order shouldBe 1
    }

    "set evaluationStrategy BY_VALUE when isValue=true" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string", isValue = true))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.evaluationStrategy shouldBe "BY_VALUE"
    }

    "set evaluationStrategy BY_REFERENCE when isValue=false" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string", isValue = false))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.evaluationStrategy shouldBe "BY_REFERENCE"
    }

    "set isVariadic to false on regular parameters" in {
      val cpg   = cpgForProgram(programWithMethod("MY_METHOD", importing = Seq(Parameter("IV_INPUT", "string"))))
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.isVariadic shouldBe false
    }

    "set sequential indices for multiple parameters" in {
      val cpg    = cpgForProgram(programWithMethod("MY_METHOD",
        importing = Seq(Parameter("IV_A", "i"), Parameter("IV_B", "i"))
      ))
      val params = cpg.method.nameExact("MY_METHOD").parameter.l.sortBy(_.index)
      params(0).name shouldBe "IV_A"
      params(0).index shouldBe 1
      params(1).name shouldBe "IV_B"
      params(1).index shouldBe 2
    }
  }

  "AstCreator for a standalone method" should {

    "create a method node with correct name" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").size shouldBe 1
    }

    "create accessible parameters via method.parameter" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(
            Parameter("IV_INPUT", "string"),
            Parameter("IV_FLAG", "abap_bool")
          )
        )
      )
      val method = cpg.method.nameExact("MY_METHOD").head
      method.parameter.size shouldBe 2
      method.parameter.name.l should contain allOf ("IV_INPUT", "IV_FLAG")
    }

    "have parameters with correct types" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(Parameter("IV_INPUT", "string"))
        )
      )
      val param = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      param.typeFullName shouldBe "string"
    }

    "have parameters with correct indices" in {
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(
            Parameter("IV_FIRST", "string"),
            Parameter("IV_SECOND", "int")
          )
        )
      )
      val params = cpg.method.nameExact("MY_METHOD").parameter.l
      params.find(_.name == "IV_FIRST").map(_.index) shouldBe Some(1)
      params.find(_.name == "IV_SECOND").map(_.index) shouldBe Some(2)
    }

    "create a method return node" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").methodReturn.size shouldBe 1
    }

    "create a method body block" in {
      val cpg = cpgForProgram(programWithMethod("MY_METHOD"))
      cpg.method.nameExact("MY_METHOD").astChildren.isBlock.size shouldBe 1
    }

    "create a local variable accessible via method.local" in {
      val body = StatementList(
        statements = Seq(
          DataDeclaration("LV_RESULT", "string", span = noSpan)
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))

      val method = cpg.method.nameExact("MY_METHOD").head
      method.local.size shouldBe 1
      method.local.name.head shouldBe "LV_RESULT"
    }
  }

  "AstCreator call node properties" should {

    "set name to method name on call" in {
      val body = StatementList(Seq(CallExpr("obj", Some("run"), span = noSpan)), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("run").size shouldBe 1
    }

    "set methodFullName as target.method for instance call" in {
      val body = StatementList(Seq(CallExpr("obj", Some("run"), isStatic = false, span = noSpan)), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("run").head
      call.methodFullName shouldBe "obj.run"
    }

    "set DYNAMIC_DISPATCH for instance call" in {
      val body = StatementList(Seq(CallExpr("obj", Some("run"), isStatic = false, span = noSpan)), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("run").head
      call.dispatchType shouldBe "DYNAMIC_DISPATCH"
    }

    "set STATIC_DISPATCH for static call" in {
      val body = StatementList(Seq(CallExpr("CL_CLASS", Some("method"), isStatic = true, span = noSpan)), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("method").head
      call.dispatchType shouldBe "STATIC_DISPATCH"
    }

    "set CLASS::method fullName for class-qualified call within class" in {
      val body = StatementList(Seq(CallExpr("", Some("helper"), span = noSpan)), noSpan)
      val cpg  = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("helper").head
      call.methodFullName shouldBe "MY_CLASS::helper"
    }

    "link call arguments via ARGUMENT edges" in {
      val body = StatementList(Seq(
        CallExpr("obj", Some("run"),
          arguments = Seq(Argument(Some("iv_x"), IdentifierExpr("lv_val", noSpan))),
          span = noSpan)
      ), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("run").head
      // receiver "obj" is argument index 0; "lv_val" is index 1
      call.argument.size shouldBe 2
      call.argument.isIdentifier.nameExact("obj").argumentIndex.head shouldBe 0
      call.argument.isIdentifier.nameExact("lv_val").size shouldBe 1
    }

    "set argumentName for named parameters" in {
      val body = StatementList(Seq(
        CallExpr("obj", Some("method"),
          arguments = Seq(
            Argument(Some("iv_name"), IdentifierExpr("lv_value", noSpan)),
            Argument(Some("iv_count"), LiteralExpr("42", "NUMBER", noSpan))
          ),
          span = noSpan)
      ), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("method").head

      // Filter out receiver (index 0) to get actual arguments
      val args = call.argument.filter(_.argumentIndex > 0).l
      args.size shouldBe 2

      // Check argument names are set
      val argNames = args.flatMap(_.argumentName).sorted
      argNames shouldBe List("iv_count", "iv_name")

      // Check codes match
      args.find(_.argumentName.contains("iv_name")).map(_.code) shouldBe Some("lv_value")
      args.find(_.argumentName.contains("iv_count")).map(_.code) shouldBe Some("42")
    }

    "handle arguments without names (positional)" in {
      val body = StatementList(Seq(
        CallExpr("obj", Some("method"),
          arguments = Seq(
            Argument(None, IdentifierExpr("lv_value", noSpan)),
            Argument(None, LiteralExpr("42", "NUMBER", noSpan))
          ),
          span = noSpan)
      ), noSpan)
      val cpg  = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val call = cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("method").head

      // Arguments without names should have empty argumentName
      val args = call.argument.filter(_.argumentIndex > 0).l
      args.size shouldBe 2
      args.forall(_.argumentName.isEmpty) shouldBe true
    }
  }

  "AstCreator literal nodes" should {

    "create a LITERAL node as child of an assignment" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_X", noSpan), LiteralExpr("42", "NUMBER", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.code.l shouldBe List("42")
    }

    "set typeFullName NUMBER on integer literals" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_X", noSpan), LiteralExpr("42", "NUMBER", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.typeFullName.l shouldBe List("NUMBER")
    }

    "set typeFullName STRING on quoted string literals" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_X", noSpan), LiteralExpr("'hello'", "STRING", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.typeFullName.l shouldBe List("STRING")
    }

    "set typeFullName STRING on string template literals (|...|)" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_X", noSpan), LiteralExpr("||", "STRING", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.code.l shouldBe List("||")
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.typeFullName.l shouldBe List("STRING")
    }

    "set code to the exact literal text" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_X", noSpan), LiteralExpr("'test value'", "STRING", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.head.code shouldBe "'test value'"
    }

    "not produce literals for abap_false / abap_true (they are identifiers)" in {
      val body = StatementList(Seq(
        AssignmentStmt(IdentifierExpr("LV_BOOL", noSpan), IdentifierExpr("abap_false", noSpan), noSpan)
      ), noSpan)
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isLiteral.l shouldBe empty
      cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("abap_false").size shouldBe 1
    }
  }

  "AstCreator for methods with body statements" should {

    "create call nodes for method calls" in {
      val body = StatementList(
        statements = Seq(
          CallExpr(targetName = "HELPER", methodName = Some("RUN"), span = noSpan)
        ),
        span = noSpan
      )
      val cpg    = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      val method = cpg.method.nameExact("MY_METHOD").head
      method.ast.isCall.size should be > 0
    }

    "create assignment call nodes" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            target = IdentifierExpr("LV_X", noSpan),
            value = LiteralExpr("42", "int", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))
      cpg.method.nameExact("MY_METHOD").ast.isCall.nameExact("<operator>.assignment").size shouldBe 1
    }
  }

  "AstCreator for a class with methods" should {

    "create a type declaration for the class" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.typeDecl.nameExact("MY_CLASS").size shouldBe 1
    }

    "create method with class-qualified full name" in {
      val cpg = cpgForProgram(programWithClass("MY_CLASS", "MY_METHOD"))
      cpg.method.fullNameExact("MY_CLASS::MY_METHOD").size shouldBe 1
    }

    "create accessible parameters for class methods" in {
      val cpg = cpgForProgram(
        programWithClass(
          "MY_CLASS",
          "MY_METHOD",
          importing = Seq(Parameter("IV_VALUE", "int"))
        )
      )
      val method = cpg.method.fullNameExact("MY_CLASS::MY_METHOD").head
      method.parameter.size shouldBe 1
      method.parameter.name.head shouldBe "IV_VALUE"
    }
  }

  "RETURNING parameter" should {

    "create a LOCAL node for the return variable" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      cpg.method.nameExact("MY_METHOD").local.name.l should contain("RV_RESULT")
    }

    "create a METHOD_RETURN node with correct type" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      val methodReturn = cpg.method.nameExact("MY_METHOD").methodReturn.head
      methodReturn.typeFullName shouldBe "string"
      methodReturn.code shouldBe "RV_RESULT"
    }

    "create a METHOD_RETURN node with BY_VALUE evaluation strategy when isValue=true" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      val methodReturn = cpg.method.nameExact("MY_METHOD").methodReturn.head
      methodReturn.evaluationStrategy shouldBe "BY_VALUE"
    }

    "create a METHOD_RETURN node with BY_REFERENCE evaluation strategy when isValue=false" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = false)))
      )
      val methodReturn = cpg.method.nameExact("MY_METHOD").methodReturn.head
      methodReturn.evaluationStrategy shouldBe "BY_REFERENCE"
    }

    "set correct typeFullName on LOCAL node" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      val local = cpg.method.nameExact("MY_METHOD").local.nameExact("RV_RESULT").head
      local.typeFullName shouldBe "string"
    }

    "not use the type name as the LOCAL name" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      cpg.method.nameExact("MY_METHOD").local.name.l should not contain "string"
    }

    "create a REF edge from an identifier referencing the return variable to the LOCAL node" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(IdentifierExpr("RV_RESULT", noSpan), LiteralExpr("42", "string", noSpan), noSpan)
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD",
          returning = Some(Parameter("RV_RESULT", "string", isValue = true)),
          body = Some(body))
      )
      val local = cpg.method.nameExact("MY_METHOD").local.nameExact("RV_RESULT").head
      cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("RV_RESULT")
        .flatMap(_._refOut).headOption shouldBe Some(local)
    }

    "handle parameter name 'return' correctly" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(IdentifierExpr("return", noSpan), LiteralExpr("value", "string", noSpan), noSpan)
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD",
          returning = Some(Parameter("return", "string", isValue = true)),
          body = Some(body))
      )
      // LOCAL node should exist with name "return"
      val local = cpg.method.nameExact("MY_METHOD").local.nameExact("return").head
      local.name shouldBe "return"
      local.typeFullName shouldBe "string"

      // Identifier should reference the LOCAL node
      val identifier = cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("return").head
      identifier._refOut.headOption shouldBe Some(local)
    }

    "not create METHOD_PARAMETER_IN for RETURNING parameter" in {
      val cpg = cpgForProgram(
        programWithMethod("MY_METHOD", returning = Some(Parameter("RV_RESULT", "string", isValue = true)))
      )
      // RETURNING should NOT create a parameter node
      cpg.method.nameExact("MY_METHOD").parameter.name.l should not contain "RV_RESULT"
    }
  }

  "RefEdgePass" should {

    "create REF edges from identifiers to parameters" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            target = IdentifierExpr("LV_RESULT", noSpan),
            value = IdentifierExpr("IV_INPUT", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(
        programWithMethod(
          "MY_METHOD",
          importing = Seq(Parameter("IV_INPUT", "string")),
          body = Some(body)
        )
      )

      val param      = cpg.method.nameExact("MY_METHOD").parameter.nameExact("IV_INPUT").head
      val identifier = cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("IV_INPUT").head
      identifier._refOut.headOption shouldBe Some(param)
    }

    "create REF edges from identifiers to locals" in {
      val body = StatementList(
        statements = Seq(
          DataDeclaration("LV_RESULT", "string", span = noSpan),
          AssignmentStmt(
            target = IdentifierExpr("LV_RESULT", noSpan),
            value = LiteralExpr("hello", "string", noSpan),
            span = noSpan
          )
        ),
        span = noSpan
      )
      val cpg = cpgForProgram(programWithMethod("MY_METHOD", body = Some(body)))

      val local = cpg.method.nameExact("MY_METHOD").local.nameExact("LV_RESULT").head
      // Identifier "LV_RESULT" on left side of assignment should ref to local
      val identifiers = cpg.method.nameExact("MY_METHOD").ast.isIdentifier.nameExact("LV_RESULT").l
      identifiers.exists(_._refOut.headOption.contains(local)) shouldBe true
    }
  }
}
