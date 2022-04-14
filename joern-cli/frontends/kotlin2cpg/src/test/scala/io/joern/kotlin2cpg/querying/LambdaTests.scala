package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.joern.kotlin2cpg.passes.Constants
import io.shiftleft.codepropertygraph.generated.edges.Capture
import io.shiftleft.codepropertygraph.generated.nodes.{Binding, ClosureBinding}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LambdaTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with a simple lambda which captures a method parameter" - {
    lazy val cpg = TestContext.buildCpg("""
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
    lazy val cpg = TestContext.buildCpg("""
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
      cpg.local.code("baz").size shouldBe 2
    }

    "should contain a CLOSURE_BINDING node for captured `baz`" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node contains REF edge to LOCAL" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).outE.size shouldBe 1
    }
  }

  "CPG for code with a list iterator lambda" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo(x: String): Int {
        |    val l = listOf("one", x)
        |    l.forEach { arg ->
        |        println(arg)
        |    }
        |    return 1
        |}
        |""".stripMargin)

    "should contain a CALL node for `println`" in {
      val callsWithPrintln = cpg.call.code(".*println.*").code.toSet
      callsWithPrintln should contain("println(arg)")
      callsWithPrintln.exists(_.trim.startsWith("l.forEach")) shouldBe true
    }

    "should contain a METHOD node for the lambda the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(14)
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "java.lang.Object"
      mr.lineNumber shouldBe Some(5)
      mr.columnNumber shouldBe Some(14)
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod) = cpg.method.fullName(".*lambda.*").modifier.l
      mod.modifierType shouldBe ModifierTypes.VIRTUAL
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "arg"
      p.lineNumber shouldBe Some(5)
      p.columnNumber shouldBe Some(16)
      p.typeFullName shouldBe "java.lang.String"
    }

    "should contain a CALL node for `forEach` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*forEach.*").l
      c.methodFullName shouldBe "java.lang.Iterable.forEach:void(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(4)

      val List(firstArg, secondArg) = cpg.call.methodFullName(".*forEach.*").argument.l
      firstArg.argumentIndex shouldBe 0
      secondArg.argumentIndex shouldBe 1
    }

    "should contain a TYPE_DECL node for the lambda with the correct props set" in {
      val List(td) = cpg.typeDecl.fullName(".*lambda.*").l
      td.isExternal shouldBe true
      td.code shouldBe ""
      td.inheritsFromTypeFullName shouldBe Seq("kotlin.Function1")
      td.astParent.size shouldBe 1

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "java.lang.Object(java.lang.Object)"
      b.name shouldBe Constants.lambdaBindingName
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with a scope function lambda" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun throughTakeIf(x: String): String? {
        |    val y = x.takeIf { arg -> arg.length > 1}
        |    return y
        |}
        |
        |fun main() {
        |    val myVal = throughTakeIf("AVALUE")
        |    println(myVal)
        |}
        |""".stripMargin)

    "should contain a METHOD node for the lambda the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "java.lang.Object"
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod) = cpg.method.fullName(".*lambda.*").modifier.l
      mod.modifierType shouldBe ModifierTypes.VIRTUAL
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "arg"
      p.typeFullName shouldBe "java.lang.String"
    }

    "should contain a CALL node for `takeIf` with the correct properties set" in {
      val List(c) = cpg.call.code("x.takeIf.*").l
      c.methodFullName shouldBe "java.lang.Object.takeIf:java.lang.Object(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.lang.String"
      c.signature shouldBe "java.lang.Object(kotlin.Function1)"
    }

    "should contain a TYPE_DECL node for the lambda with the correct props set" in {
      val List(td) = cpg.typeDecl.fullName(".*lambda.*").l
      td.isExternal shouldBe true
      td.code shouldBe ""
      td.astParent.size shouldBe 1

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "java.lang.Object(java.lang.Object)"
      b.name shouldBe Constants.lambdaBindingName
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with a lambda mapping values of a collection" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun mappedListWith(p: String): List<String> {
        |    val col = listOf("one", p)
        |    val mappedCol = col.map { arg ->
        |          arg + "MAPPED"
        |      }
        |    return mappedCol
        |}
        |
        |fun main() {
        |  val x = mappedListWith("AVALUE")
        |  println(x)
        |}
        |""".stripMargin)

    "should contain a METHOD node for the lambda the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(28)
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "java.lang.Object"
      mr.lineNumber shouldBe Some(5)
      mr.columnNumber shouldBe Some(28)
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod) = cpg.method.fullName(".*lambda.*").modifier.l
      mod.modifierType shouldBe ModifierTypes.VIRTUAL
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "arg"
      p.typeFullName shouldBe "java.lang.String"
    }

    "should contain a CALL node for `map` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*map.*").take(1).l
      c.methodFullName shouldBe "java.lang.Iterable.map:java.util.List(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(20)
    }

    "should contain a TYPE_DECL node for the lambda with the correct props set" in {
      val List(td) = cpg.typeDecl.fullName(".*lambda.*").l
      td.isExternal shouldBe true
      td.code shouldBe ""

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><f_generated.kt_no1>:java.lang.Object(java.lang.Object)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "java.lang.Object(java.lang.Object)"
      b.name shouldBe Constants.lambdaBindingName
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with destructuring inside lambda" - {
    lazy val cpg = TestContext.buildCpg("""
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

  "CPG for code with a simple lambda which captures a method parameter inside method" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass {
        |    fun doSomething(x: String) {
        |        1.let {
        |            println(x)
        |        }
        |    }
        |}
        |
        |""".stripMargin)

    "should contain CALL node for println" in {
      cpg.call.code("print.*").size shouldBe 1
    }

    "should contain a METHOD node for the lambda with the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with a simple lambda which captures a method parameter, nested twice" - {
    lazy val cpg = TestContext.buildCpg("""
      |package mypkg
      |
      |fun foo(x: String): Int {
      |    1.let {
      |      2.let {
      |        println(x)
      |      }
      |    }
      |   return 0
      |}
      |""".stripMargin)

    "should contain two METHOD nodes representing the lambdas" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 2
    }

    "should contain a METHOD node for the second lambda with the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*2.*").l
      m.signature shouldBe "java.lang.Object(java.lang.Object)"
    }
  }

  "CPG for code with call with lambda inside method definition" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |class AClass() {
        |    fun check(col: List<Int?>) {
        |        val rand = Random.nextInt(0, 100)
        |        when (rand) {
        |            1 -> println("1!")
        |            2 -> println("2!")
        |            else -> {
        |                val filtered = col.all { entry -> entry != null }
        |                println(filtered)
        |            }
        |        }
        |    }
        |}
        |
        |fun main() {
        |    val list = listOf(1, 2, 3)
        |    val a = AClass()
        |    a.check(list)
        |    println("SCOPE")
        |}
        |""".stripMargin)

    "should a METHOD node for the lambda" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }
  }
}
