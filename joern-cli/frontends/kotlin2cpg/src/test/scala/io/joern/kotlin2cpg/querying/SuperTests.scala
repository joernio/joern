package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SuperTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple call using _super_" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |open class BClass {
        |   open fun myfun() {
        |       println("B.myfun")
        |   }
        |}
        |
        |class AClass : BClass() {
        |    override fun myfun() {
        |        super.myfun()
        |        println("A.myfun")
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass()
        |    a.myfun()
        |}
        |
        |""".stripMargin)

    "should have a CALL node for the call using _super_ with the correct props set" in {
      val List(c) = cpg.call.code("super.*").l
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.methodFullName shouldBe "mypkg.BClass.myfun:void()"
      c.signature shouldBe "void()"
      c.lineNumber shouldBe Some(11)
      c.columnNumber shouldBe Some(8)
    }
  }
}
