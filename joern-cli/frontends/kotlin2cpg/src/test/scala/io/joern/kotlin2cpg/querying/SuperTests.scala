package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SuperTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple call using _super_" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.methodFullName shouldBe "mypkg.BClass.myfun:kotlin.Unit()"
      c.signature shouldBe "kotlin.Unit()"
      c.lineNumber shouldBe Some(11)
      c.columnNumber shouldBe Some(8)
    }
  }
}
