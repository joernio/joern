package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ScopeFunctionTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple `let` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = "n"
        |  1.let {
        |    println(x)
        |  }
        |""".stripMargin)

    "should contain a CALL node for the `println` invocation inside the scope function body" in {
      cpg.call.code("println.*").size should not be 0
    }

    "should contain a reference from the `x` declaration to the identifier referenced in the scope function body" in {
      cpg.local.nameExact("x").referencingIdentifiers.l.size should not be 0
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*.lambda.*").l should not be 0
    }

    "should contain a METHOD node for the lambda with the package name part of METHOD_FULL_NAME" in {
      cpg.method.fullName(".*mypkg.*lambda.*").size shouldBe 1
    }

    // TODO: add the implicit _it_ param to the signature
    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with simple `run` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = "n"
        |  1.run {
        |    println(x)
        |  }
        |}
        |""".stripMargin)

    "should contain a CALL node for the `println` invocation inside the scope function body" in {
      cpg.call.code("println.*").size should not be 0
    }

    "should contain a reference from the `x` declaration to the identifier referenced in the scope function body" in {
      cpg.local.nameExact("x").referencingIdentifiers.l.size should not be 0
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*.lambda.*").l should not be 0
    }

    "should contain a METHOD node for the lambda with the package name part of METHOD_FULL_NAME" in {
      cpg.method.fullName(".*mypkg.*lambda.*").size shouldBe 1
    }

    // TODO: add the implicit _this_ param to the signature
    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with simple `also` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = "n"
        |  1.also {
        |    println(x)
        |  }
        |}
        |""".stripMargin)

    "should contain a CALL node for the `println` invocation inside the scope function body" in {
      cpg.call.code("println.*").size should not be 0
    }

    "should contain a reference from the `x` declaration to the identifier referenced in the scope function body" in {
      cpg.local.nameExact("x").referencingIdentifiers.l.size should not be 0
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*.lambda.*").l should not be 0
    }

    "should contain a METHOD node for the lambda with the package name part of METHOD_FULL_NAME" in {
      cpg.method.fullName(".*mypkg.*lambda.*").size shouldBe 1
    }

    // TODO: add the implicit _this_ param to the signature
    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with `with` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo(x: String): Int {
        |    val numbers = mutableListOf("one", "two", "three")
        |    with(numbers) {
        |      println("'with' is called with argument $this, it contains $size elements")
        |    }
        |}
        |""".stripMargin)

    "should contain a CALL node for `println`" in {
      cpg.call.code("println.*").size shouldBe 1
    }

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }
  }

  "CPG for code with simple `apply` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Bar(p: String)
        |
        |fun foo() {
        |  val x: String = "n"
        |  val b = Bar("", "").apply { p = x }
        |  println(b.p)
        |}
        |""".stripMargin)

    // TODO: add test for lowering
  }

  "CPG for code with simple `takeIf` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: Int = 3
        |  val y = x.takeIf { it -> it % 2 == 0 }
        |  println(y)
        |}
        |""".stripMargin)

    // TODO: add test case for CALLs inside the scope function
    // at the moment, they don't end up in the CPG correctly
  }

  "CPG for code with simple `takeUnless` scope function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: Int = 3
        |  val y = x.takeUnless { it -> it % 2 == 0 }
        |  println(y)
        |}
        |""".stripMargin)

    // TODO: add test case for CALLs inside the scope function
    // at the moment, they don't end up in the CPG correctly
  }

}
