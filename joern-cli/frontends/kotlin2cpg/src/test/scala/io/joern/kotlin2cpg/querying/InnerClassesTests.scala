package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Ignore

class InnerClassesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a simple inner class declaration" should {
    val cpg = code("""package box
        |class Jungle { inner class Rumble { fun title() = "Ali v. Foreman" } }
        |fun main() {
        |    val m = Jungle().Rumble().title()
        |    println(m) // prints `Ali v. Foreman`
        |}
        |""".stripMargin)

    "contain a TYPE_DECL node with the correct FULL_NAME" in {
      val List(td) = cpg.typeDecl.nameExact("Rumble").l
      td.fullName shouldBe "box.Jungle$Rumble"
    }

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.codeExact("Jungle().Rumble().title()").l
      c.methodFullName shouldBe "box.Jungle$Rumble.title:java.lang.String()"
    }
  }
}
