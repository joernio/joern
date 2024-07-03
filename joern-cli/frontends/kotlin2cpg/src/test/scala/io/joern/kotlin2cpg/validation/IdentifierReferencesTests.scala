package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*

// TODO: also add test with refs inside TYPE_DECL

class IdentifierReferencesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with shadowed local inside lambda" should {
    lazy val cpg = code("""
        |package main
        |
        |fun main() {
        |    val x: Int? = 41414141
        |    val out =
        |      x.takeIf { in1 ->
        |        val x: Int? = 42424242
        |        val cmp = in1.takeIf { in2 ->
        |            x!! >= in2!!
        |        }
        |        cmp != null
        |    }
        |    println(out) // prints 41414141
        |}
        |""".stripMargin)

    "should contain LOCAL nodes with correctly-set referencing IDENTIFIERS" in {
      val List(outerScopeX: Local) = cpg.local.nameExact("x").whereNot(_.closureBindingId).take(1).l
      outerScopeX.referencingIdentifiers.size shouldBe 2
      outerScopeX.referencingIdentifiers.lineNumber.l shouldBe List(5, 7)

      val List(innerScopeX: Local) = cpg.local.nameExact("x").whereNot(_.closureBindingId).slice(1, 2).l
      innerScopeX.referencingIdentifiers.size shouldBe 1
      innerScopeX.referencingIdentifiers.lineNumber.l shouldBe List(8)
    }
  }

  "CPG for code with identifier referencing two possible locals" should {
    lazy val cpg = code("""
        |fun foo() {
        |  val x: String = "n"
        |  1.let {
        |    val x: Int = 1
        |    println(x)
        |  }
        |}
        |""".stripMargin)

    "should contain a local inside the scope function with two referencing identifiers" in {
      cpg.local
        .nameExact("x")
        .typeFullName("int")
        .referencingIdentifiers
        .size shouldBe 2
    }

    "should contain a local outside the scope function with a single referenced identifier" in {
      cpg.local
        .nameExact("x")
        .typeFullName("java.lang.String")
        .referencingIdentifiers
        .size shouldBe 1
    }
  }

  "CPG for code with locals with the same name, but in different scopes" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |    val x = "FIRST"
        |    if (true) {
        |        val x ="SECOND"
        |        if (true) {
        |            val x = "THIRD"
        |            println("third: " + x)
        |        } else {
        |            println("second: " + x)
        |        }
        |    }
        |    println("first: " + x)
        |}
        |""".stripMargin)

    "should contain LOCAL nodes with correctly-set referencing IDENTIFIERS" in {
      val List(firstX: Local, secondX: Local, thirdX: Local) = cpg.local.nameExact("x").l
      val List(firstXUsage: Identifier) =
        cpg.call.methodFullName(Operators.addition).code(".*first.*").argument(2).l: @unchecked
      firstX.referencingIdentifiers.id.l.contains(firstXUsage.id) shouldBe true
      firstXUsage.refsTo.size shouldBe 1

      val List(secondXUsage: Identifier) =
        cpg.call.methodFullName(Operators.addition).code(".*second.*").argument(2).l: @unchecked
      secondX.referencingIdentifiers.id.l.contains(secondXUsage.id) shouldBe true
      secondXUsage.refsTo.size shouldBe 1

      val List(thirdXUsage: Identifier) =
        cpg.call.methodFullName(Operators.addition).code(".*third.*").argument(2).l: @unchecked
      thirdX.referencingIdentifiers.id.l.contains(thirdXUsage.id) shouldBe true
      thirdXUsage.refsTo.size shouldBe 1
    }
  }

  "should find references for simple case" should {
    lazy val cpg = code("""
        |fun foo(x: Int): Int {
        |  val y = x
        |  return y
        |}
        |""".stripMargin)

    "identifiers to the parameters exist" in {
      val param = cpg.method.name("foo").parameter.name("x").head
      param.referencingIdentifiers.toSet should not be Set()
    }

    "identifiers to the locals exist" in {
      val localNode = cpg.method.name("foo").local.name("y").head
      localNode.referencingIdentifiers.toSet should not be Set()
    }
  }

  "should find references inside expressions" should {
    lazy val cpg = code("""
        |fun foo(x: Int): Int {
        |  val y = 1 + x
        |  return y
        |}
        |""".stripMargin)

    "identifiers to the parameters exist" in {
      val param = cpg.method.name("foo").parameter.name("x").head
      param.referencingIdentifiers.toSet should not be Set()
    }
  }

  "CPG for code with a call to `also` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.also { it + 41414141 }
        |}
        |""".stripMargin)

    "should contain a REF edge for the IDENTIFIER referencing the implicit _it_ parameter" in {
      val List(itIdentifier) = cpg.identifier.nameExact("it").l
      itIdentifier.refsTo.size shouldBe 1
      val List(methodParam) = itIdentifier.refsTo.collectAll[MethodParameterIn].l
      methodParam.name shouldBe "it"
    }
  }

  "CPG for code with a call to `apply` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.apply { this + 41414141 }
        |}
        |""".stripMargin)

    "should contain a REF edge for the IDENTIFIER referencing the implicit _this_ parameter" in {
      val List(thisIdentifier) = cpg.identifier.nameExact("this").l
      thisIdentifier.refsTo.size shouldBe 1
      val List(methodParam) = thisIdentifier.refsTo.collectAll[MethodParameterIn].l
      methodParam.name shouldBe "this"
    }
  }

  "CPG for code with a call to `let` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.let { it + 41414141 }
        |}
        |""".stripMargin)

    "should contain a REF edge for the IDENTIFIER referencing the implicit _this_ parameter" in {
      val List(itIdentifier) = cpg.identifier.nameExact("it").l
      itIdentifier.refsTo.size shouldBe 1
      val List(methodParam) = itIdentifier.refsTo.collectAll[MethodParameterIn].l
      methodParam.name shouldBe "it"
    }
  }

  "CPG for code call to `run` scope function without an explicitly-defined parameter" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  1.run { this + 41414141 }
        |}
        |""".stripMargin)

    "should contain a REF edge for the IDENTIFIER referencing the implicit _this_ parameter" in {
      val List(thisIdentifier) = cpg.identifier.nameExact("this").l
      thisIdentifier.refsTo.size shouldBe 1
      val List(methodParam) = thisIdentifier.refsTo.collectAll[MethodParameterIn].l
      methodParam.name shouldBe "this"
    }
  }
}
