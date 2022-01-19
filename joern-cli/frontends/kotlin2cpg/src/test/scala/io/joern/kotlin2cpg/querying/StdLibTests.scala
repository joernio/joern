package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StdLibTests extends AnyFreeSpec with Matchers {
  "CPG for code with call to `takeIf`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |  package mypkg
        |
        |  import kotlin.random.Random
        |  import java.util.UUID
        |
        |  fun main() {
        |    val r = Random.nextInt(0, 100)
        |    val x =
        |      if(r < 50) {
        |        null
        |      } else {
        |        UUID.randomUUID()
        |      }
        |    val p = x.takeIf { it != null }
        |    println(p)
        |  }
        |""".stripMargin)

    "should contain a CALL node with the correct METHOD_FULL_NAME for `takeIf`" in {
      val List(c) = cpg.call.code("x.takeIf.*").l
      // TODO: erase types
      c.methodFullName shouldBe "java.util.UUID.takeIf:java.util.UUID((java.util.UUID)->kotlin.Boolean)"
    }
  }

  "CPG for code with a single call to println and a corresponding import" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.io.println
        |
        |fun foo() {
        |  println("bar")
        |}
        |""".stripMargin)

    "should contain a CALL node with the correct METHOD_FULL_NAME" in {
      val List(c) = cpg.call.code("println.*").l
      c.methodFullName shouldBe "kotlin.io.println:kotlin.Unit(kotlin.Any)"
    }
  }

  "CPG for code with a single call to println, a corresponding import and a locally defined println method" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun println(baz: String) {
        |  print("prefix: " + baz + "\n")
        |}
        |
        |fun foo() {
        |  println("bar")
        |}
        |""".stripMargin)

    "should contain a CALL node with the correct METHOD_FULL_NAME" in {
      val List(c) = cpg.call.code("println.*").l
      c.methodFullName shouldBe "mypkg.println:kotlin.Unit(kotlin.String)"
    }
  }

  "CPG for code with a call to static class method of imported class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val runtime = Runtime.getRuntime()
        |  runtime.exec("ls")
        |}
        |""".stripMargin)

    "should contain a CALL node for call to static class method" in {
      cpg.call.code("Runtime.getRuntime\\(\\)").size shouldBe 1
    }

    "should contain a CALL node for call to instance method" in {
      cpg.call.code("runtime.exec.*").size shouldBe 1
    }

    "should contain a LOCAL node with an inferred TYPE_FULL_NAME set" in {
      val List(l) = cpg.local.code(".*runtime.*").l
      l.typeFullName shouldBe "java.lang.Runtime"
    }
  }

  "CPG for code with a chained call to static class method of imported class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  Runtime.getRuntime().exec("ls")
        |}
        |""".stripMargin)

    "should contain a CALL node for call to instance method" in {
      val List(c) = cpg.call.code(".*exec.*").l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(kotlin.String)"
    }
  }

  "CPG for code with a call to infix fn `to`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key4" to 1)
        |  println(numbersMap)
        |}
        |""".stripMargin)

    "should contain CALL nodes for calls to infix fn `to`" in {
      val List(c1) = cpg.call.code("\"key1.*").l
      c1.methodFullName shouldBe "kotlin.to:kotlin.Pair(kotlin.Int)"

      val List(c2) = cpg.call.code("\"key2.*").l
      c2.methodFullName shouldBe "kotlin.to:kotlin.Pair(kotlin.Int)"

      val List(c3) = cpg.call.code("\"key3.*").l
      c3.methodFullName shouldBe "kotlin.to:kotlin.Pair(kotlin.Int)"

      val List(c4) = cpg.call.code("\"key4.*").l
      c4.methodFullName shouldBe "kotlin.to:kotlin.Pair(kotlin.Int)"
    }

    "CPG for code with local of type MutableMap and optional in it" - {
      lazy val cpg = Kt2CpgTestContext.buildCpg("""
          |package mypkg
          |
          |fun foo() {
          |  val payload: MutableMap<String, Any?> = HashMap()
          |  println(payload)
          |}
          |""".stripMargin)

      "should contain CALL node with a MFN without optionals in it" in {
        val List(c) = cpg.call.code("HashMap.*").l
        c.typeFullName shouldBe "kotlin.collections.HashMap"
        c.methodFullName shouldBe "kotlin.collections.HashMap.<init>:java.util.HashMap()"
      }
    }

    "CPG for code with calls to stdlib's `split`s" - {
      lazy val cpg = Kt2CpgTestContext.buildCpg("""
          |package mypkg
          |
          |fun main() {
          |    val foo = "one,two,three".split(",")
          |    println(foo)
          |
          |    val bar = "one,two,three".split(",", "t", ignoreCase = false)
          |    println(bar)
          |}
          |""".stripMargin)

      "should contain CALL nodes for `split` with the correct MFNs set" in {
        cpg.call.methodFullName(".*split.*").methodFullName.toSet shouldBe
          Set(
            "kotlin.CharSequence.split:kotlin.collections.List(kotlin.Array,kotlin.Boolean,kotlin.Int)"
          )
      }
    }

    "CPG for code with calls to stdlib's `trim`s" - {
      lazy val cpg = Kt2CpgTestContext.buildCpg("""
          |package mypkg
          |
          |fun trimParam(p: String): String {
          |    val y = p.trim()
          |    return y
          |}
          |
          |fun main() {
          |    val out = trimParam(" hello ")
          |    println(out)
          |}
          |
          |""".stripMargin)

      "should contain a CALL node for `trim` with the correct props set" in {
        val List(c) = cpg.call.code("p.trim.*").l
        c.methodFullName shouldBe "kotlin.String.trim:kotlin.String()"
        c.signature shouldBe "kotlin.String()"
        c.typeFullName shouldBe "kotlin.String"
        c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        c.lineNumber shouldBe Some(4)
        c.columnNumber shouldBe Some(12)
      }

      "should contain a CALL node for `trim` a receiver arg with the correct props set" in {
        val List(receiverArg) = cpg.call.code("p.trim.*").argument(0).isIdentifier.l
        receiverArg.name shouldBe "p"
        receiverArg.code shouldBe "p"
        receiverArg.typeFullName shouldBe "kotlin.String"
        receiverArg.lineNumber shouldBe Some(4)
        receiverArg.columnNumber shouldBe Some(12)
      }
    }
  }
}
