package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SpecialOperatorsTests extends AnyFreeSpec with Matchers {

  "CPG for code with _safe call_ operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun main(args : Array<String>) {
        |    val b: String? = null
        |    println(b?.length)
        |}
        |""".stripMargin)

    "should contain a call node with safe call details erased" in {
      val List(c) = cpg.call.code("b.*length.*").l
      c.code shouldBe "b?.length"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.methodFullName shouldBe Operators.fieldAccess
      c.lineNumber shouldBe Some(3)
      c.columnNumber shouldBe Some(12)
    }
  }

  "CPG for code with _as_ operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun main(args : Array<String>) {
        |    val b = "PLACEHOLDER" as String
        |    println(b)
        |}
        |""".stripMargin)

    "should contain a call node with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(Operators.cast).l
      c.code shouldBe "\"PLACEHOLDER\" as String"
      c.columnNumber shouldBe Some(12)
      c.lineNumber shouldBe Some(2)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "CPG for code with `notNullAssert` operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun foo(x : Int) {
        | val p = x!!
        | println(p)
        |}
        |""".stripMargin)

    "should contain call to `<operator>.notNullAssert`" in {
      cpg.call.methodFullNameExact(Operators.notNullAssert).size shouldBe 1
    }
  }

  "CPG for code with _notNullAssert_ operator inside dot-qualified expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun foo() {
        |    val bar = " PLACEHOLDER "!!.trim()
        |    println(bar)
        |}
        |""".stripMargin)

    "should contain a call node for `notNullAssert` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(Operators.notNullAssert).l
      c.code shouldBe "\" PLACEHOLDER \"!!"
      c.columnNumber shouldBe Some(14)
      c.lineNumber shouldBe Some(2)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "CPG for code with _is_ expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |packge mypkg
        |
        |fun main() {
        |  val o = "PLACEHOLDER"
        |  val foo = o is String
        |  println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for the _is_ expression with the correct properties set" in {
      val List(c) =
        cpg.call
          .methodFullNameExact(Operators.assignment)
          .where(_.argument(1).code("foo"))
          .argument(2)
          .isCall
          .l
      c.code shouldBe "o is String"
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(12)
      c.methodFullName shouldBe Operators.is
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "CPG for code with range expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |packge mypkg
        |
        |fun main() {
        |  val foo = 0..2
        |  println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for the _range_ expression with the correct properties set" in {
      val List(c) =
        cpg.call
          .methodFullNameExact(Operators.range)
          .where(_.argument(1).codeExact("0"))
          .isCall
          .l
      c.code shouldBe "0..2"
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(12)
      c.methodFullName shouldBe Operators.range
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "CPG for code with simple usage of _elvis_ operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = args[1]?.length ?: -1
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for elvis operator with the correct properties set" in {
      val List(c) = cpg.call.methodFullNameExact(Operators.elvis).l
      c.code shouldBe "args[1]?.length ?: -1"
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(14)
      c.methodFullName shouldBe Operators.elvis
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument.size shouldBe 2
    }

    "should contain an IDENTIFIER node for  " in {
      val List(i) = cpg.identifier.nameExact("foo").head.l
      i.typeFullName shouldBe "java.lang.Integer"
    }
  }

  "CPG for code with _elvis_ operator usage and subexpression" - {
    lazy val cpg = TestContext.buildCpg("""
        |package main
        |
        |fun main() {
        |    val valueStr = "41414141"
        |    val isValid = valueStr?.toIntOrNull() ?: valueStr?.toLongOrNull() != null
        |    println("isValid: " + isValid)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node for the result of the elvis operator call with the correct TYPE_FULL_NAME set" in {
      val List(i) = cpg.identifier.nameExact("isValid").head.l
      i.typeFullName shouldBe "java.lang.Boolean"
    }
  }

  "CPG for code with _notIn_ operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = 1 !in 0..10
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for the notIn operator with the correct properties set" in {
      val List(c) = cpg.call.methodFullNameExact(Operators.notIn).l
      c.code shouldBe "1 !in 0..10"
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(14)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument.size shouldBe 2
    }
  }

  "CPG for code with _in_ operator" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = 1 in 0..10
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for the notIn operator with the correct properties set" in {
      val List(c) = cpg.call.methodFullNameExact(Operators.in).l
      c.code shouldBe "1 in 0..10"
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(14)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument.size shouldBe 2
    }
  }
}
