package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef}
import io.shiftleft.semanticcpg.language.*

class CallbackTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with callback and additional parameter" should {
    val cpg = code("""
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
      cpg.method.fullName(".*lambda.*").isLambda.size shouldBe 1
    }

    "METHOD node should have the correct number of parameters" in {
      cpg.method.fullName(".*lambda.*").isLambda.parameter.size shouldBe 1
    }

    "method parameter should have the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "token"
      p.lineNumber shouldBe Some(10)
      p.columnNumber shouldBe Some(22)
    }

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }
  }

  "CPG for code with simple callback usage" should {
    val cpg = code("""
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

  "CPG for code with simple callback inside class method" should {
    val cpg = code("""
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
      c.lineNumber shouldBe Some(10)
      c.columnNumber shouldBe Some(8)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument.size shouldBe 2
      inside(c.argument.l) { case List(arg1: Identifier, arg2: MethodRef) =>
        arg1.name shouldBe "this"
        arg1.argumentIndex shouldBe 0
        arg2.argumentIndex shouldBe 1
      }
    }
  }
}
