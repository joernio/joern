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

    "should contain two CALL nodes with identical CODE,MFN & DISPATCH_TYPE props" in {
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

    "should contain an ANNOTATION node attached to the annotated class" in {
      val List(td)    = cpg.typeDecl.nameExact("HealthController").l
      def annotations = td.astChildren.collect { case c: Annotation => c }
      annotations.size shouldBe 1
      val List(annotation) = annotations.l
      annotation.code shouldBe "@Controller"
      annotation.name shouldBe "Controller"
      annotation.fullName shouldBe "mypkg.Controller"
    }

    "should contain an ANNOTATION node attached to the annotated method" in {
      val List(m)     = cpg.method.nameExact("health").l
      def annotations = m.astChildren.collect { case c: Annotation => c }
      annotations.size shouldBe 1
      val List(annotation) = annotations.l
      annotation.code shouldBe "@Route(\"/health\")"
      annotation.name shouldBe "Route"
      annotation.fullName shouldBe "mypkg.Route"
    }

    "should contain an ANNOTATION node for the annotated parameter" in {
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
}
