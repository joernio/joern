package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Capture
import io.shiftleft.codepropertygraph.generated.nodes.ClosureBinding
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.jIteratortoTraversal

class LambdaTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with a simple lambda which captures a method parameter" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.List
        |
        |fun foo(x: String): Int {
        |    1.let {
        |       println(x)
        |    }
        |   return 0
        |}
        |""".stripMargin)

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }

    "should contain a capture edge for the methodRef" in {
      cpg.methodRef.outE.filter(_.isInstanceOf[Capture]).size shouldBe 1
    }

    "should contain a LOCAL node for the captured `x`" in {
      cpg.local.code("x").size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node for captured `x`" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node contains REF edge to METHOD_PARAMETER_IN" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).outE.size shouldBe 1
    }
  }

  "CPG for code with a simple lambda which captures a local" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.List
        |
        |fun foo(x: String): Int {
        |    val baz: String = "PLACEHOLDER"
        |    1.let {
        |       println(baz)
        |    }
        |   return 0
        |}
        |""".stripMargin)

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }

    "should contain a capture edge for the methodRef" in {
      cpg.methodRef.outE.filter(_.isInstanceOf[Capture]).size shouldBe 1
    }

    "should contain a LOCAL node for the captured `baz`" in {
      cpg.local.code("baz").size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node for captured `baz`" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node contains REF edge to LOCAL" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).outE.size shouldBe 1
    }
  }

  "CPG for code with a list iterator lambda" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.List
        |import kotlin.collections.listOf
        |
        |fun foo(x: String): Int {
        |    val l: kotlin.collections.List = listOf("one", x)
        |    l.forEach { it ->
        |       println(it)
        |    }
        |   return y
        |}
        |""".stripMargin)

    "should contain a CALL node for `println`" in {
      cpg.call.code(".*println.*").size shouldBe 1
    }

    "should contain a METHOD node for the lambda with the package name part of METHOD_FULL_NAME" in {
      cpg.method.fullName(".*mypkg.*lambda.*").size shouldBe 1
    }

    "should contain a CALL node for `forEach` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*forEach.*").l
      c.methodFullName shouldBe "kotlin.Any.forEach:ANY(ANY)"
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "it"
      p.lineNumber shouldBe Some(8)
      p.columnNumber shouldBe Some(16)
      p.typeFullName shouldBe "ANY"
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with destructuring inside lambda" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
       |package mypkg
       |
       |fun main(args: Array<String>) {
       |    val map = mapOf("one" to 1, "two" to 2, "three" to 3)
       |    map.mapValues { (key, value) -> "$value!" }
       |}
       |""".stripMargin)

    "should not contain any method parameters with an empty name" in {
      cpg.method.parameter.filter { p => p.name == null }.method.fullName.l shouldBe Seq()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter passed to the ctor of class inside sealed class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.List
        |
        |sealed class X {
        |  class P(val x: String)
        |}
        |
        |internal open class Z {
        |  internal inline fun updateState(x: String) {
        |    println("state updated")
        |  }
        |}
        |
        |class Y : Z {
        |  fun foo(bar: String): Int {
        |      updateState {
        |        X.P(bar)
        |      }
        |     return 0
        |  }
        |}
        |""".stripMargin)

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }

    "should not contain any locals without incoming AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
}
