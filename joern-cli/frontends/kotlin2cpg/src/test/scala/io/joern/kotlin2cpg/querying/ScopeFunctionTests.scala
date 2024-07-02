package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Return}
import io.shiftleft.semanticcpg.language.*

class ScopeFunctionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code call to `also` scope function without an explicitly-defined parameter" should {
    val cpg = code("fun f1(p: String) { p.also { println(it) } }")

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _it_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "it"
    }

    "should NOT contain a RETURN node around as the last child of the lambda's BLOCK" in {
      val List(b: Block) = cpg.method.fullName(".*lambda.*").block.l
      val hasReturnAsLastChild = b.astChildren.last match {
        case _: Return => true
        case _         => false
      }
      hasReturnAsLastChild shouldBe false
    }
  }

  "CPG for code with call to `apply` scope function without an explicitly-defined parameter" should {
    val cpg = code("fun f1(p: String) { p.apply { println(this) } }")

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "this"
    }

    "should NOT contain a RETURN node around as the last child of the lambda's BLOCK" in {
      val List(b: Block) = cpg.method.fullName(".*lambda.*").block.l
      val hasReturnAsLastChild = b.astChildren.last match {
        case _: Return => true
        case _         => false
      }
      hasReturnAsLastChild shouldBe false
    }
  }

  "CPG for code call to `let` scope function without an explicitly-defined parameter" should {
    val cpg = code("fun f1(p: String) { p.let { println(it) } }")

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "it"
    }

    "should contain a RETURN node around as the last child of the lambda's BLOCK" in {
      val List(b: Block) = cpg.method.fullName(".*lambda.*").block.l
      val hasReturnAsLastChild = b.astChildren.last match {
        case _: Return => true
        case _         => false
      }
      hasReturnAsLastChild shouldBe true
    }
  }

  "CPG for code call to `run` scope function without an explicitly-defined parameter" should {
    val cpg = code("fun f1(p: String) { p.run { println(this) } }")

    "should contain a METHOD_PARAMETER_IN node for the implicit parameter _this_" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.name shouldBe "this"
    }

    "should contain a RETURN node around as the last child of the lambda's BLOCK" in {
      val List(b: Block) = cpg.method.fullName(".*lambda.*").block.l
      val hasReturnAsLastChild = b.astChildren.last match {
        case _: Return => true
        case _         => false
      }
      hasReturnAsLastChild shouldBe true
    }
  }

  "CPG for code with simple `let` scope function" should {
    val cpg = code("""
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
    val cpg = code("""
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
    val cpg = code("""
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
    val cpg = code("""
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

  "CPG for code with simple `takeIf` scope function" should {
    val cpg = code("""
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
