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

  "CPG for code with an annotation on a simple qualified expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1.toString()
      |}
      |""".stripMargin)

    "contain an ANNOTATION node for the annotation" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a safe qualified expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1?.toString()
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

  "CPG for code with an annotation on an if-expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy if (true) println("this") else println("that")
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on an is-expression" should {
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

  "CPG for code with an annotation on a labeled expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy albel@ 1
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a parenthesized expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy() (1)
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy()").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a prefix expression" should {
    val cpg = code("""
        |package mypkg
        |@Target(AnnotationTarget.EXPRESSION)
        |@Retention(AnnotationRetention.SOURCE)
        |annotation class Fancy
        |fun fn1() {
        |    @Fancy ++1
        |}
        |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a postfix expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy 1++
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a string template" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy "something"
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a try expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy try { "a" } catch (e: Exception) { "b" }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a super expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |class A {
      |   fun fn1() {
      |       println(@Fancy super.toString())
      |   }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a this expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |class A {
      |   fun fn1() {
      |       println(@Fancy this.toString())
      |   }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a simple identifier expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    val i = 1
      |    @Fancy i
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a do-while control structure" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    var i = 10
      |    @Fancy
      |    do {
      |        println("tick")
      |        --i
      |    } while (i > 0)
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a while control structure" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    var i = 10
      |    @Fancy
      |    while (i > 0) {
      |        println("tick")
      |        --i
      |    }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a for control structure" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy
      |    for (i in 1..10) {
      |        println("tick")
      |    }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a when control structure" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy
      |    when (Random(1).nextInt() % 2) {
      |        1 -> println("this")
      |        else -> println("that")
      |    }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a when expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |  val x =
      |    @Fancy
      |    when (Random(1).nextInt() % 2) {
      |        1 -> "this"
      |        else -> "that"
      |    }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a local declaration" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy
      |    var i = 1
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on an array access expression" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    val l = arrayOf(1, 2, 3)
      |    @Fancy
      |    l[0]
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on a lambda" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy
      |    { println("this") }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with an annotation on an anonymous function" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Fancy
      |fun fn1() {
      |    @Fancy
      |    fun() { println("this") }
      |}
      |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@Fancy").size shouldBe 1
    }
  }

  "CPG for code with a custom annotation" should {
    val cpg = code("""
        |package mypkg
        |import retrofit2.http.POST
        |
        |interface Username {
        |    @Headers("Content-Type: application/json")
        |    @POST("/name")
        |    fun sendUsername(@Body username: UserDto): Call<Void>
        |}
        |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@POST(\"/name\")").size shouldBe 1
    }

    "the ANNOTATION node should have correct full name" in {
      cpg.all.collectAll[Annotation].codeExact("@POST(\"/name\")").fullName.head shouldBe "retrofit2.http.POST"
    }
  }
}
