package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ClassLiteralTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple class literals" - {

    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Bar {}
        |class Baz {}
        |
        |fun foo() {
        |  println(Bar::class)
        |  println(Baz::class.java)
        |}
        |""".stripMargin)

    "should contain a CALL node for the class literal expression" in {
      val List(p) = cpg.call("getClass").code("Bar.*").l
      p.argument.size shouldBe 0
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      p.code shouldBe "Bar::class"
      p.columnNumber shouldBe Some(10)
      p.lineNumber shouldBe Some(7)
      p.signature shouldBe "java.lang.Class()"
    }

    "should contain a CALL node for the class literal expression inside dot-qualified expression" in {
      val List(p) = cpg.call("getClass").code("Baz.*").l
      p.argument.size shouldBe 0
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      p.code shouldBe "Baz::class"
      p.columnNumber shouldBe Some(10)
      p.lineNumber shouldBe Some(8)
      p.signature shouldBe "java.lang.Class()"
    }
  }
}
