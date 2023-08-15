package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, AnnotationLiteral}
import io.shiftleft.semanticcpg.language._

class AnnotationsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with two identical calls, one annotated and one not" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |    @Suppress("DEPRECATION")
        |    println("AMESSAGE")
        |
        |    println("AMESSAGE")
        |}
        |""".stripMargin)

    "contain two CALL nodes with identical CODE,METHOD_FULL_NAME & DISPATCH_TYPE props" in {
      val List(c1, c2) = cpg.call.code("println.*").l
      c1.code shouldBe c2.code
      c1.methodFullName shouldBe c2.methodFullName
      c1.dispatchType shouldBe c2.dispatchType
    }
  }

  "CPG for code with an annotated class" should {
    val cpg = code("""|package mypkg
        |
        |annotation class Controller
        |annotation class Route(val path: String)
        |annotation class RequestParam(val name: String)
        |
        |@Controller
        |class HealthController {
        |    @Route("/health")
        |    fun health(): String {
        |        return "ok"
        |    }
        |
        |    @GetMapping("/greet")
        |    fun greet(@RequestParam("username") username: String): String {
        |      return "Greetings ${username}!"
        |    }
        |}
        |""".stripMargin)

    "contain an ANNOTATION node attached to the annotated class" in {
      val List(td)    = cpg.typeDecl.nameExact("HealthController").l
      def annotations = td.astChildren.collect { case c: Annotation => c }
      annotations.size shouldBe 1
      val List(annotation) = annotations.l
      annotation.code shouldBe "@Controller"
      annotation.name shouldBe "Controller"
      annotation.fullName shouldBe "mypkg.Controller"
    }

    "contain an ANNOTATION node attached to the annotated method" in {
      val List(m)     = cpg.method.nameExact("health").l
      def annotations = m.astChildren.collect { case c: Annotation => c }
      annotations.size shouldBe 1
      val List(annotation) = annotations.l
      annotation.code shouldBe "@Route(\"/health\")"
      annotation.name shouldBe "Route"
      annotation.fullName shouldBe "mypkg.Route"
    }

    "contain an ANNOTATION node for the annotated parameter" in {
      val List(p)     = cpg.method.parameter.nameExact("username").l
      def annotations = p.astChildren.collect { case c: Annotation => c }
      annotations.size shouldBe 1
      val List(annotation) = annotations.l
      annotation.code shouldBe "@RequestParam(\"username\")"
      annotation.name shouldBe "RequestParam"
      annotation.fullName shouldBe "mypkg.RequestParam"

      val List(annotationLiteral) = annotation.astChildren.collectAll[AnnotationLiteral].l
      annotationLiteral.code shouldBe "\"username\""
    }
  }

  "CPG for code with an annotation on a simple call" should {
    val cpg = code("""
        |package mypkg
        |@Target(AnnotationTarget.EXPRESSION)
        |@Retention(AnnotationRetention.SOURCE)
        |annotation class Fancy
        |fun fn1() {
        |    @Fancy println("something")
        |}
        |""".stripMargin)

    "contain an ANNOTATION node for the annotation" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a simple ctor call" should {
    val cpg = code("""
        |package mypkg
        |class X(val p: String)
        |@Target(AnnotationTarget.EXPRESSION)
        |@Retention(AnnotationRetention.SOURCE)
        |annotation class Fancy
        |fun fn1() {
        |    @Fancy X("something")
        |}
        |""".stripMargin)

    "contain an ANNOTATION node for the annotation" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a literal" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a binary expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1 + 1
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a binary expression with type RHS" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1 is Int
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a simple class declaration" should {
    val cpg = code("""
       |package mypkg
       |@Target(AnnotationTarget.CLASS)
       |@Retention(AnnotationRetention.SOURCE)
       |annotation class Fancy
       |@Fancy class A
       |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on an object declaration" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.CLASS)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |@Fancy object O {}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on an object expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.CLASS)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy object {}
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a class literal expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |class A
      |fun fn1() {
      |    @Fancy A::class
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }
}
