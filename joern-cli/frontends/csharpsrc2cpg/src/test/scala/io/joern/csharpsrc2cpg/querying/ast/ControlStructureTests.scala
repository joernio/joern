package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends CSharpCode2CpgFixture {

  "the throw statement" should {
    val cpg = code(basicBoilerplate("""
        |throw new Exception("Error!");
        |""".stripMargin))

    "create a throw operation with exception constructor" in {
      inside(cpg.call.nameExact(CSharpOperators.throws).headOption) {
        case Some(x: Call) =>
          x.methodFullName shouldBe CSharpOperators.throws
          x.code shouldBe "throw new Exception(\"Error!\");"
          x.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

          inside(x.argumentOption(1)) {
            case Some(exp: Call) =>
              exp.code shouldBe "new Exception(\"Error!\")"
              exp.name shouldBe Defines.ConstructorMethodName
              exp.typeFullName shouldBe "Exception" // TODO: Should really be System.Exception
            case _ => fail("No exception constructor node found!")
          }

        case None => fail("No 'throw' call node found!")
      }
    }
  }

  "exception handling statements" should {

    val cpg = code(basicBoilerplate("""
        |var Busy = true;
        |try
        |{
        |  Console.WriteLine("Hello");
        |}
        |catch (Exception e)
        |{
        |  Console.WriteLine("Uh, oh!");
        |} finally
        |{
        | Busy = false;
        |}
        |""".stripMargin))

    "generate a try-block with the correct console call" in {}

  }

}
