package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallbackTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with callback and additional parameter" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun withCallback(p: String, callback: (String) -> Unit) {
        |    println("before callback")
        |    callback(p)
        |}
        |
        |fun foo(x: String) {
        |    withCallback(x) { token ->
        |       println(token)
        |    }
        |}
        |""".stripMargin)

    "should contain two CALL node for `println`" in {
      cpg.call.code("println.*").size shouldBe 2
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }

    "METHOD node should have the correct number of parameters" in {
      cpg.method.fullName(".*lambda.*").parameter.size shouldBe 1
    }

    "method parameter should have the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "token"
      p.lineNumber shouldBe Some(9)
      p.columnNumber shouldBe Some(22)
    }

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }
  }

  "CPG for code with simple callback usage" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |fun withCallback(fn: (String)->Unit) {
      |    fn("FROM_INSIDE")
      |}
      |
      |fun main() {
      |    println("printing with callback")
      |    val msg = "FROM_OUTSIDE"
      |    withCallback { x ->
      |        println(x)
      |    }
      |}
      |""".stripMargin)

    "should contain CALL nodes for both `println` invocations" in {
      cpg.call.code("println.*").size shouldBe 2
    }
  }

  "CPG for code with simple callback inside class method" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |class AClass {
      |    private fun withCallback(fn: (String)->Unit) {
      |        fn("MESSAGE_2")
      |    }
      |
      |    fun printWithCallback(msg: String) {
      |        withCallback { x ->
      |            println(msg)
      |            println(x)
      |        }
      |    }
      |}
      |
      |fun main() {
      |    println("Running")
      |    val a = AClass()
      |    a.printWithCallback("MESSAGE_1")
      |}
      |""".stripMargin)

    "should contain a CALL node for the invocation of method with callback with the correct props" in {
      val List(c) = cpg.call.methodFullName(".*withCallback.*").l
      c.lineNumber shouldBe Some(9)
      c.columnNumber shouldBe Some(8)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      c.argument.size shouldBe 1
    }
  }
}
