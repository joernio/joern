package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class ScopeFunctionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code call to `also` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.also { it }
        |}
        |""".stripMargin)

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _this_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "it"
    }
  }

  "CPG for code call to `apply` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.apply { this }
        |}
        |""".stripMargin)

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _this_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "this"
    }
  }

  "CPG for code call to `let` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.let { it }
        |}
        |""".stripMargin)

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _it_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "it"
    }
  }

  "CPG for code call to `run` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.run { this }
        |}
        |""".stripMargin)

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _this_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "this"
    }
  }

  "CPG for code with simple `let` scope function" should {
    lazy val cpg = code("""
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

    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with simple `run` scope function" should {
    lazy val cpg = code("""
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

    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with simple `also` scope function" should {
    lazy val cpg = code("""
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

    "should contain a METHOD node with the correct signature" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with `with` scope function" should {
    lazy val cpg = code("""
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

  "CPG for code with simple `apply` scope function" should {
    lazy val cpg = code("""
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

  "CPG for code with simple `takeIf` scope function" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  val x: Int = 3
        |  val y = x.takeIf { it -> it % 2 == 0 }
        |  println(y)
        |}
        |""".stripMargin)

    "should X" in {
      val List(c) = cpg.call.code("x.takeIf.*").l
      c.methodFullName shouldBe "java.lang.Object.takeIf:java.lang.Object(kotlin.Function1)"
    }
  }
}
