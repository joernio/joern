package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.{CCodeToCpgSuite, DataFlowCodeToCpgSuite}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Method}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class MacroHandlingTests extends CCodeToCpgSuite {

  "MacroHandlingTests1" should {
    val cpg = code("""
       |#define A_MACRO(x,c) (x = 10 + c)
       |int foo() {
       |int *y;
       |A_MACRO(*y, 2);
       |return 10 * y;
       |}
    """.stripMargin)

    "should correctly expand macro" in {
      val List(x: Call) = cpg.method("foo").call.nameExact(Operators.assignment).l
      x.code shouldBe "*y = 10 + 2"
      x.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      val List(macroCall: Call) = cpg.method("foo").call.nameExact("A_MACRO").l
      macroCall.code shouldBe "A_MACRO(*y, 2)"
      val List(arg1: Call, arg2: Literal) = macroCall.argument.l.sortBy(_.order)
      arg1.name shouldBe Operators.indirection
      arg1.code shouldBe "*y"
      arg1.order shouldBe 1
      arg1.argumentIndex shouldBe 1
      val List(identifier: Identifier) = arg1.argument.l
      identifier.name shouldBe "y"
      identifier.order shouldBe 1
      arg2.code shouldBe "2"
      arg2.order shouldBe 2
      arg2.argumentIndex shouldBe 2
    }

    "should create a METHOD for the expanded macro" in {
      val List(m: Method) = cpg.method.name("A_MACRO").l
      m.fullName.endsWith("A_MACRO:2") shouldBe true
      m.filename.endsWith(".c") shouldBe true
      m.lineNumber shouldBe Some(2)
      m.lineNumberEnd shouldBe Some(2)
      val List(param1, param2) = m.parameter.l.sortBy(_.order)
      param1.name shouldBe "p1"
      param2.name shouldBe "p2"
    }

  }

  "MacroHandlingTests2" should {
    val cpg = code("""
       |#define A_MACRO(x) (x = A_SECOND_MACRO(x))
       |#define A_SECOND_MACRO(x) (x+1)
       |int foo() {
       | int y;
       | A_MACRO(y);
       | return 10 * y;
       |}
    """.stripMargin)

    "should correctly expand macro inside macro" in {
      val List(x: Call) = cpg.method("foo").call.nameExact(Operators.assignment).l
      x.code shouldBe "y = (y + 1)"
      val List(identifier: Identifier, call2: Call) = x.astChildren.l.sortBy(_.order)
      identifier.code shouldBe "y"
      call2.code shouldBe "y + 1"
      val List(arg1: Identifier, arg2: Literal) = call2.argument.l.sortBy(_.argumentIndex)
      arg1.name shouldBe "y"
      arg1.order shouldBe 1
      arg1.argumentIndex shouldBe 1
      arg2.code shouldBe "1"
      arg2.order shouldBe 2
      arg2.argumentIndex shouldBe 2
    }
  }

  "MacroHandlingTests3" should {
    val cpg = code("""
       |#define A_MACRO(x) (printf(x))
       |int foo() {
       |int y;
       |A_MACRO(y);
       |return 10 * y;
       |}
    """.stripMargin)

    "should correctly expand macro inside macro" in {
      val List(x: Call) = cpg.method("foo").call.nameExact("printf").l
      x.code shouldBe "printf(y)"
      val List(arg: Identifier) = x.argument.l.sortBy(_.argumentIndex)
      arg.name shouldBe "y"
    }
  }

  "MacroHandlingTests4" should {
    val cpg = code("""
       |#define A_MACRO(dst, code, size)\
     do \
    { \
        if( (i_read) >= (size) ) \
        { \
            dst = (code); \
            p_peek += (size); \
            i_read -= (size); \
        } \
        else \
        { \
            dst = 0; \
            i_read = 0; \
        } \
    } while(0)
       |
       |#define A_MACRO_2() (dst++)
       |
       |int foo() {
       |char * dst, ptr;
       |A_MACRO(dst, ptr, 1);
       |dst++;
       |A_MACRO_2();
       |return 10 * y;
       |}
    """.stripMargin)

    "should correctly include calls to macros" in {
      val List(call1: Call) = cpg.method("foo").call.nameExact("A_MACRO").l
      call1.code shouldBe "A_MACRO(dst, ptr, 1)"
      call1.name shouldBe "A_MACRO"
      call1.methodFullName.endsWith("A_MACRO:3") shouldBe true
      call1.lineNumber shouldBe Some(22)
      call1.columnNumber shouldBe Some(1)
      call1.typeFullName shouldBe "ANY"
      call1.dispatchType shouldBe DispatchTypes.INLINED
      val List(arg1, arg2, arg3) = call1.argument.l.sortBy(_.order)
      arg1.code shouldBe "dst"
      arg2.code shouldBe "ptr"
      arg3.code shouldBe "1"

      val List(call2: Call) = cpg.method("foo").call.nameExact("A_MACRO_2").l
      call2.code shouldBe "A_MACRO_2()"
      call2.name shouldBe "A_MACRO_2"
      call2.methodFullName.endsWith("A_MACRO_2:0") shouldBe true
      call2.lineNumber shouldBe Some(24)
      call2.columnNumber shouldBe Some(1)
      call2.typeFullName shouldBe "ANY"
      call2.argument.l.sortBy(_.order).size shouldBe 0
      call2.dispatchType shouldBe DispatchTypes.INLINED
    }
  }

  "MacroHandlingTests5" should {
    val cpg = code("""
       |#define A_MACRO (1)
       |int foo() {
       |  return A_MACRO;
       |}
    """.stripMargin)

    "should correctly include call to macro that is only a constant in a return" in {
      val List(call: Call) = cpg.method("foo").call.l
      call.name shouldBe "A_MACRO"
      call.code shouldBe "A_MACRO"
      call.argument.size shouldBe 0
    }
  }

  "MacroHandlingTests6" should {
    val cpg = code("""
       |#define A_MACRO 0x0
       |int foo() {
       |  return A_MACRO;
       |}
    """.stripMargin)

    "should correctly include call to macro that is only a constant (no brackets) in a return" in {
      val List(call: Call) = cpg.method("foo").call.l
      call.name shouldBe "A_MACRO"
      call.code shouldBe "A_MACRO"
      call.argument.size shouldBe 0
    }

  }
}

class CfgMacroTests extends DataFlowCodeToCpgSuite {
  private val cpg = code("""
       |#define MP4_GET4BYTES( dst ) MP4_GETX_PRIVATE( dst, GetDWBE(p_peek), 4 )
       |#define MP4_GETX_PRIVATE(dst, code, size) \
    do \
    { \
        if( (i_read) >= (size) ) \
        { \
            dst = (code); \
            p_peek += (size); \
            i_read -= (size); \
        } \
        else \
        { \
            dst = 0; \
            i_read = 0; \
        } \
    } while(0)

    |int foo() {
    |unsigned int x;
    |MP4_GET4BYTES(x);
    |sink(x);
    |}""".stripMargin)

  "should create correct CFG for macro expansion and find data flow" in {
    val List(callToMacro: Call) = cpg.method("foo").call.dispatchType(DispatchTypes.INLINED).l
    callToMacro.argument.code.l shouldBe List("x")
    callToMacro.cfgNext.code.toSetMutable shouldBe Set("x", "i_read")
    val source = cpg.method("foo").call.name("MP4_GET4BYTES").argument(1).l
    val sink   = cpg.method("foo").call.name("sink").argument(1).l
    sink.reachableByFlows(source).l.foreach(println)
  }
}
