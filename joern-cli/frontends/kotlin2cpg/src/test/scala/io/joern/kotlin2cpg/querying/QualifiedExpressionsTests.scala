package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class QualifiedExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with qualified expression with QE as a receiver" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val execStr = "touch /tmp/kt2cpg-test-case.txt"
        |    Runtime.getRuntime().exec(execStr)
        |}
        |""".stripMargin)

    "should contain a CALL node for the long DQE with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code("Runtime.getRuntime\\(\\).exec\\(execStr\\)").l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(java.lang.String)"
    }

    "should contain a CALL node for the DQE inside the DQE with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code("Runtime.getRuntime\\(\\)").l
      c.methodFullName shouldBe "java.lang.Runtime.getRuntime:java.lang.Runtime()"
    }
  }

  "CPG for code with chained qualified expressions" should {
    val cpg = code("""
        |fun main() {
        |      val out = StringBuilder().append("one").append("-two").toString()
        |      println(out)
        |}
        |""".stripMargin)

    "should contain a CALL node for the long DQE with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.codeExact("StringBuilder().append(\"one\")").l
      c.methodFullName shouldBe "java.lang.StringBuilder.append:java.lang.StringBuilder(java.lang.String)"
    }
  }

  "CPG for code with qualified expression with CALL as a receiver" should {
    val cpg = code("""
        |package mypkg
        |
        |fun getHashMap(): HashMap<String,String> {
        |   val aMap = HashMap<String, String>()
        |   aMap["user"] = "foo"
        |   return aMap
        |}
        |
        |fun foo() {
        |  val contains = getHashMap().containsKey("user")
        |  val value = getHashMap()["user"]
        |  println(contains)
        |  println(value)
        |}
        |""".stripMargin)

    "should contain a CALL node for the `.*containsKey.*` QE with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code(".*containsKey.*").methodFullNameNot(Operators.assignment).l
      c.methodFullName shouldBe "java.util.HashMap.containsKey:boolean(java.lang.Object)"
    }

    "should contain a CALL node for the `.*containsKey.*` QE with the correct arguments set" in {
      def parentCall     = cpg.call.code(".*containsKey.*").methodFullNameNot(Operators.assignment)
      val List(firstArg) = parentCall.argument(0).isCall.l
      firstArg.code shouldBe "getHashMap()"

      val List(secondArg) = parentCall.argument(1).isLiteral.l
      secondArg.code shouldBe "\"user\""
    }
  }

  "CPG for code with qualified expression with `when` as receiver" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun main(args: Array<String>) {
        |    val r = Random.nextInt(0, 100)
        |    val foo = when {
        |        r in 0..50 -> true
        |        r in 51..100 -> false
        |        else -> false
        |    }.toString()
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for QE's selector with the correct METHOD_FULL_NAME set" in {
      val List(c) =
        cpg.call.methodFullName(Operators.assignment).where(_.argument(1).code(".*foo.*")).argument(2).isCall.l
      c.methodFullName shouldBe "kotlin.Boolean.toString:java.lang.String()"
    }
  }

  "CPG for code with qualified expression in which the receiver is a call to array access" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |    val arr = arrayOf(1, 2, 3)
        |    val z = arr[0].toString()
        |    print(z)
        |}
        |""".stripMargin)

    "should contain a CALL node with the first argument a CALL with the correct props set" in {
      val List(c) = cpg.call.code("arr.*toString.*").l
      c.methodFullName shouldBe "kotlin.Int.toString:java.lang.String()"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = cpg.call.code("arr.*toString.*").argument.isCall.l
      receiver.argumentIndex shouldBe 0
      receiver.code shouldBe "arr[0]"
      receiver.methodFullName shouldBe Operators.indexAccess
      receiver.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
