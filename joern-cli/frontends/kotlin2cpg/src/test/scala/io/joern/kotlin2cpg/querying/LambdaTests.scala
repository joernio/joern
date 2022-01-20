package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
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
        |fun foo(x: String): Int {
        |    val l = listOf("one", x)
        |    l.forEach { arg ->
        |        println(arg)
        |    }
        |    return 1
        |}
        |""".stripMargin)

    "should contain a CALL node for `println`" in {
      cpg.call.code(".*println.*").size shouldBe 1
    }

    "should contain a METHOD node for the lambda the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*").l
      m.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      m.signature shouldBe "kotlin.Any(kotlin.Any)"
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(14)
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "kotlin.Any"
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
      p.typeFullName shouldBe "kotlin.String"
    }

    "should contain a CALL node for `forEach` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*forEach.*").l
      c.methodFullName shouldBe "kotlin.collections.Iterable.forEach:kotlin.Unit(kotlin.Function1)"
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

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "kotlin.Any(kotlin.Any)"
      b.name shouldBe Constants.lambdaBindingName
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with a scope function lambda" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      m.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      m.signature shouldBe "kotlin.Any(kotlin.Any)"
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "kotlin.Any"
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod) = cpg.method.fullName(".*lambda.*").modifier.l
      mod.modifierType shouldBe ModifierTypes.VIRTUAL
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "arg"
      p.typeFullName shouldBe "kotlin.String"
    }

    "should contain a CALL node for `takeIf` with the correct properties set" in {
      val List(c) = cpg.call.code("x.takeIf.*").l
      c.methodFullName shouldBe "kotlin.String.takeIf:kotlin.String(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a TYPE_DECL node for the lambda with the correct props set" in {
      val List(td) = cpg.typeDecl.fullName(".*lambda.*").l
      td.isExternal shouldBe true
      td.code shouldBe ""

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "kotlin.Any(kotlin.Any)"
      b.name shouldBe Constants.lambdaBindingName
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with referencing identifiers" in {
      cpg.method.fullName(".*lambda.*").parameter.referencingIdentifiers.size shouldBe 1
    }
  }

  "CPG for code with a lambda mapping values of a collection" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      m.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      m.signature shouldBe "kotlin.Any(kotlin.Any)"
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(28)
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "kotlin.Any"
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
      p.typeFullName shouldBe "kotlin.String"
    }

    "should contain a CALL node for `map` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*map.*").take(1).l
      c.methodFullName shouldBe "kotlin.collections.Iterable.map:kotlin.collections.List(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(20)
    }

    "should contain a TYPE_DECL node for the lambda with the correct props set" in {
      val List(td) = cpg.typeDecl.fullName(".*lambda.*").l
      td.isExternal shouldBe true
      td.code shouldBe ""

      val List(bm) = cpg.typeDecl.fullName(".*lambda.*").boundMethod.l
      bm.fullName shouldBe "mypkg.<lambda><no1>:kotlin.Any(kotlin.Any)"
      bm.name shouldBe Constants.lambdaName

      val List(b) = bm.refIn.collect { case r: Binding => r }.l
      b.signature shouldBe "kotlin.Any(kotlin.Any)"
      b.name shouldBe Constants.lambdaBindingName
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

  "CPG for code with a simple lambda which captures a method parameter inside method" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      m.signature shouldBe "kotlin.Any(kotlin.Any)"
    }
  }

  "CPG for code with a simple lambda which captures a method parameter, nested twice" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      m.signature shouldBe "kotlin.Any(kotlin.Any)"
    }
  }
}
