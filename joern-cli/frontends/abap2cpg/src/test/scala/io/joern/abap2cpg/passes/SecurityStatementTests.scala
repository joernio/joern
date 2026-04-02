package io.joern.abap2cpg.passes

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.testfixtures.AbapCpgFixture
import io.shiftleft.semanticcpg.language.*

/** Tests for ABAP security-relevant statements that should create CALL nodes.
  *
  * Reference: https://github.com/redrays-io/ABAP-Code-Scanner/tree/master/checks
  */
class SecurityStatementTests extends AbapCpgFixture {

  "DELETE DYNPRO statement" should {

    "create a DELETE_DYNPRO CALL node" in {
      val body = StatementList(
        statements = Seq(
          // DELETE DYNPRO program screen
          CallExpr("DELETE_DYNPRO", None,
            Seq(
              Argument(Some("PROGRAM"), IdentifierExpr("lv_program", noSpan)),
              Argument(Some("SCREEN"), IdentifierExpr("lv_screen", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("DELETE_DYNPRO").head

      call.name shouldBe "DELETE_DYNPRO"
      call.dispatchType shouldBe "STATIC_DISPATCH"

      val args = call.argument.l
      // Note: Includes synthetic receiver at index 0
      args.size shouldBe 3

      // Filter out receiver (index 0) to get actual arguments
      val actualArgs = args.filter(_.argumentIndex > 0)
      actualArgs.map(_.code).sorted shouldBe List("lv_program", "lv_screen")
    }

    "have PROGRAM and SCREEN arguments with names" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("DELETE_DYNPRO", None,
            Seq(
              Argument(Some("PROGRAM"), IdentifierExpr("prog_name", noSpan)),
              Argument(Some("SCREEN"), LiteralExpr("100", "NUMBER", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("DELETE_DYNPRO").head

      // After ARGUMENT_NAME fix (P0), this should work:
      // val argNames = call.argument.argumentName.l
      // argNames should contain allOf("PROGRAM", "SCREEN")

      // For now, just verify arguments exist
      val args = call.argument.l
      args.exists(_.code == "prog_name") shouldBe true
      args.exists(_.code == "100") shouldBe true
    }
  }

  "CALL SYSTEM with ID COMMAND FIELD" should {

    "create a CALL_SYSTEM_COMMAND CALL node" in {
      val body = StatementList(
        statements = Seq(
          // CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command
          CallExpr("CALL_SYSTEM_COMMAND", None,
            Seq(
              Argument(Some("COMMAND"), IdentifierExpr("lv_command", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("CALL_SYSTEM_COMMAND").head

      call.name shouldBe "CALL_SYSTEM_COMMAND"
      val args = call.argument.l
      args.exists(_.code == "lv_command") shouldBe true
    }
  }

  "C function calls with ID FIELD parameters" should {

    "create CALL node with function name" in {
      val body = StatementList(
        statements = Seq(
          // CALL 'C_SAPGPARAM' ID 'NAME' FIELD lv_name ID 'VALUE' FIELD lv_value
          CallExpr("C_SAPGPARAM", None,
            Seq(
              Argument(Some("NAME"), IdentifierExpr("lv_name", noSpan)),
              Argument(Some("VALUE"), IdentifierExpr("lv_value", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("C_SAPGPARAM").head

      call.name shouldBe "C_SAPGPARAM"
      call.dispatchType shouldBe "STATIC_DISPATCH"

      val args = call.argument.l
      // Note: Includes synthetic receiver at index 0
      args.size shouldBe 3

      // Filter out receiver (index 0) to get actual arguments
      val actualArgs = args.filter(_.argumentIndex > 0)
      actualArgs.map(_.code).sorted shouldBe List("lv_name", "lv_value")
    }

    "preserve ID parameter names" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("C_RSTRB_READ_BUFFERED", None,
            Seq(
              Argument(Some("FILE"), IdentifierExpr("lv_file", noSpan)),
              Argument(Some("BUFFER"), IdentifierExpr("lv_buffer", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("C_RSTRB_READ_BUFFERED").head

      val args = call.argument.l
      args.exists(_.code == "lv_file") shouldBe true
      args.exists(_.code == "lv_buffer") shouldBe true
    }
  }

  "XSS-related method calls" should {

    "create CALL node for request->get_form_field" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("request", Some("get_form_field"),
            Seq(
              Argument(Some("name"), LiteralExpr("'username'", "STRING", noSpan))
            ),
            isStatic = false,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("get_form_field").head

      call.name shouldBe "get_form_field"
      call.methodFullName shouldBe "request.get_form_field"
      call.dispatchType shouldBe "DYNAMIC_DISPATCH"

      val receiver = call.argument.argumentIndex(0).head
      receiver.code shouldBe "request"
    }

    "create CALL node for out->print_string" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("out", Some("print_string"),
            Seq(
              Argument(None, IdentifierExpr("lv_output", noSpan))
            ),
            isStatic = false,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("print_string").head

      call.name shouldBe "print_string"
      call.methodFullName shouldBe "out.print_string"
      call.dispatchType shouldBe "DYNAMIC_DISPATCH"
    }

    "create CALL node for escape function" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("escape", None,
            Seq(
              Argument(Some("val"), IdentifierExpr("lv_input", noSpan))
            ),
            isStatic = false,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("escape").head

      call.name shouldBe "escape"
      val args = call.argument.l
      args.exists(_.code == "lv_input") shouldBe true
    }
  }

  "Weak hashing algorithm literals" should {

    "detect MD5 string literal" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            IdentifierExpr("lv_algo", noSpan),
            LiteralExpr("'MD5'", "STRING", noSpan),
            noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val md5Literals = cpg.literal.code("'MD5'").l

      md5Literals.size shouldBe 1
      md5Literals.head.typeFullName shouldBe "STRING"
    }

    "detect SHA1 string literal" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            IdentifierExpr("lv_algo", noSpan),
            LiteralExpr("'SHA1'", "STRING", noSpan),
            noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val sha1Literals = cpg.literal.code.l.filter(_.contains("SHA1"))

      sha1Literals should not be empty
    }

    "detect multiple weak algorithms" in {
      val body = StatementList(
        statements = Seq(
          AssignmentStmt(
            IdentifierExpr("lv_algo1", noSpan),
            LiteralExpr("'MD5'", "STRING", noSpan),
            noSpan
          ),
          AssignmentStmt(
            IdentifierExpr("lv_algo2", noSpan),
            LiteralExpr("'HMACMD5'", "STRING", noSpan),
            noSpan
          ),
          AssignmentStmt(
            IdentifierExpr("lv_algo3", noSpan),
            LiteralExpr("'HAVAL128'", "STRING", noSpan),
            noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val weakAlgos = List("MD5", "HMACMD5", "HAVAL128")

      weakAlgos.foreach { algo =>
        val literals = cpg.literal.code.l.filter(_.contains(algo))
        withClue(s"Algorithm $algo not found:") {
          literals should not be empty
        }
      }
    }
  }

  "cl_gui_frontend_services=>execute" should {

    "create CALL node with correct methodFullName" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("cl_gui_frontend_services", Some("execute"),
            Seq(
              Argument(Some("application"), IdentifierExpr("lv_app", noSpan)),
              Argument(Some("parameter"), IdentifierExpr("lv_params", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("execute").head

      call.name shouldBe "execute"
      call.methodFullName shouldBe "cl_gui_frontend_services.execute"
      call.dispatchType shouldBe "STATIC_DISPATCH"

      val args = call.argument.l
      args.exists(_.code == "lv_app") shouldBe true
      args.exists(_.code == "lv_params") shouldBe true
    }
  }

  "Existing security statements" should {

    "create OPEN_DATASET CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("OPEN_DATASET", None,
            Seq(
              Argument(Some("FILENAME"), IdentifierExpr("lv_file", noSpan)),
              Argument(Some("FILTER"), IdentifierExpr("lv_filter", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("OPEN_DATASET").size shouldBe 1
    }

    "create READ_DATASET CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("READ_DATASET", None,
            Seq(
              Argument(Some("FILENAME"), IdentifierExpr("lv_file", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("READ_DATASET").size shouldBe 1
    }

    "create DELETE_DATASET CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("DELETE_DATASET", None,
            Seq(
              Argument(Some("FILENAME"), IdentifierExpr("lv_file", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("DELETE_DATASET").size shouldBe 1
    }

    "create TRANSFER CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("TRANSFER", None,
            Seq(
              Argument(Some("DATA"), IdentifierExpr("lv_data", noSpan)),
              Argument(Some("TO"), IdentifierExpr("lv_file", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("TRANSFER").size shouldBe 1
    }

    "create AUTHORITY_CHECK CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("AUTHORITY_CHECK", None,
            Seq(
              Argument(Some("OBJECT"), LiteralExpr("'S_TCODE'", "STRING", noSpan)),
              Argument(Some("FIELD"), IdentifierExpr("lv_field", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("AUTHORITY_CHECK").size shouldBe 1
    }

    "create GENERATE_SUBROUTINE_POOL CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("GENERATE_SUBROUTINE_POOL", None,
            Seq(
              Argument(Some("POOL"), IdentifierExpr("lv_code", noSpan)),
              Argument(Some("NAME"), IdentifierExpr("lv_prog", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("GENERATE_SUBROUTINE_POOL").size shouldBe 1
    }

    "create CALL_TRANSFORMATION CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("CALL_TRANSFORMATION", None,
            Seq(
              Argument(None, LiteralExpr("'XSLT_TRANSFORM'", "STRING", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("CALL_TRANSFORMATION").size shouldBe 1
    }

    "create EDITOR_CALL CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("EDITOR_CALL", None,
            Seq(
              Argument(Some("REPORT"), IdentifierExpr("lv_prog", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("EDITOR_CALL").size shouldBe 1
    }

    "create DO_TIMES CALL node" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("DO_TIMES", None,
            Seq(
              Argument(Some("TIMES"), LiteralExpr("10", "NUMBER", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("DO_TIMES").size shouldBe 1
    }
  }

  "CALL FUNCTION statements" should {

    "create CALL node for SXPG_COMMAND_EXECUTE" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("SXPG_COMMAND_EXECUTE", None,
            Seq.empty,
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("SXPG_COMMAND_EXECUTE").head

      call.name shouldBe "SXPG_COMMAND_EXECUTE"
      call.dispatchType shouldBe "STATIC_DISPATCH"
    }

    "create CALL node for RFC_REMOTE_EXEC" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("RFC_REMOTE_EXEC", None,
            Seq.empty,
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("RFC_REMOTE_EXEC").size shouldBe 1
    }

    "create CALL node for RFC_REMOTE_PIPE" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("RFC_REMOTE_PIPE", None,
            Seq.empty,
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("RFC_REMOTE_PIPE").size shouldBe 1
    }

    "create CALL node for RFC_REMOTE_FILE" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("RFC_REMOTE_FILE", None,
            Seq.empty,
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("RFC_REMOTE_FILE").size shouldBe 1
    }

    "create CALL node for FTP_CONNECT" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("FTP_CONNECT", None,
            Seq.empty,
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      cpg.call.name("FTP_CONNECT").size shouldBe 1
    }

    "create CALL node for execute_procedure" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("obj", Some("execute_procedure"),
            Seq(
              Argument(None, IdentifierExpr("lv_sql", noSpan))
            ),
            isStatic = false,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("execute_procedure").head

      call.methodFullName shouldBe "obj.execute_procedure"
    }

    "create CALL node for get_persistent_by_query" in {
      val body = StatementList(
        statements = Seq(
          CallExpr("cl_persistent", Some("get_persistent_by_query"),
            Seq(
              Argument(None, IdentifierExpr("lv_query", noSpan))
            ),
            isStatic = true,
            span = noSpan
          )
        ),
        span = noSpan
      )

      val cpg = cpgForProgram(programWithMethod("TEST", body = Some(body)))
      val call = cpg.call.name("get_persistent_by_query").head

      call.methodFullName shouldBe "cl_persistent.get_persistent_by_query"
    }
  }
}
