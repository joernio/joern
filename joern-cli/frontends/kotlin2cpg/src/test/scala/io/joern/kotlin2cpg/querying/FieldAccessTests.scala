package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._

class FieldAccessTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple filed access of user-defined class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class AClass {
        |    var m = "PLACEHOLDER"
        |}
        |
        |fun main() {
        |    val a = AClass()
        |    println(a.m)
        |    a.m = "VALUE"
        |    println(a.m)
        |}
        |""".stripMargin)

    "should contain a CALL node for the _fieldAccess_ expression with the correct props set" in {
      val List(c) = cpg.call.methodFullNameExact(Operators.assignment).code("a.m.*").argument(1).isCall.l
      c.methodFullName shouldBe Operators.fieldAccess
      c.code shouldBe "a.m"
      c.typeFullName shouldBe "java.lang.String"
      c.lineNumber shouldBe Some(10)
      c.columnNumber shouldBe Some(4)
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a CALL node for the _fieldAccess_ expression with the correct arguments set" in {
      val List(oneArg: Identifier, anotherArg: FieldIdentifier) =
        cpg.call.methodFullNameExact(Operators.assignment).code("a.m.*").argument(1).isCall.argument.l

      oneArg.argumentIndex shouldBe 1
      oneArg.code shouldBe "a"
      oneArg.typeFullName shouldBe "mypkg.AClass"

      anotherArg.argumentIndex shouldBe 2
      anotherArg.code shouldBe "m"
      anotherArg.canonicalName shouldBe "m"
    }
  }
}
