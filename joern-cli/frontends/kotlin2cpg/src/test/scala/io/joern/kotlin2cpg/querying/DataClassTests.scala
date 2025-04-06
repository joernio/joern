package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DataClassTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with data class" should {

    "should contain METHOD nodes for the implicit componentX-methods of the data class" in {
      val cpg = code("""
          |package mypkg
          |
          |data class Result(val p: Int, val q: String)
          |fun main() {
          |  val x = Result(41414141, "AMESSAGE")
          |  println(x.p)
          |  println(x.q)
          |}
          |""".stripMargin)

      val List(firstMethod, secondMethod) = cpg.typeDecl.nameExact("Result").method.name("component.*").l

      firstMethod.name shouldBe "component1"
      firstMethod.fullName shouldBe "mypkg.Result.component1:int()"
      firstMethod.signature shouldBe "int()"
      Option(firstMethod.block).isDefined shouldBe true

      secondMethod.name shouldBe "component2"
      secondMethod.fullName shouldBe "mypkg.Result.component2:java.lang.String()"
      secondMethod.signature shouldBe "java.lang.String()"
      Option(secondMethod.block).isDefined shouldBe true
    }

    "should contain METHOD nodes for data classes in when clauses" in {
      val cpg = code("""
          |package mypkg
          |
          |class Foo {
          |  fun main() = when (condition) {
          |    Bar.member.access -> {
          |      data class Data(var x: String? = null)
          |      Data("foo")
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg.method.size shouldBe 9
      cpg.method.methodReturn.size shouldBe 9
      val List(firstMethod) = cpg.typeDecl.nameExact("Data").method.name("component.*").l
      firstMethod.fullName shouldBe "mypkg.Foo.main.Data.component1:java.lang.String()"
      firstMethod.methodReturn.typeFullName shouldBe "java.lang.String"
    }

  }

}
