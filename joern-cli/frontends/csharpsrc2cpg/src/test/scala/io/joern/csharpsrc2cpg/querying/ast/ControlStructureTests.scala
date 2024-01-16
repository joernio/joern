package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Local}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes}
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

    val tryBlocks = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.TRY).astChildren.isBlock.l

    "generate a try control structure with three children blocks" in {
      inside(tryBlocks) {
        case t :: c :: f :: Nil =>
          t.code shouldBe "try"
          c.code shouldBe "catch"
          f.code shouldBe "finally"
        case _ => fail("Invalid number of children under the `try` control structure!")
      }
    }

    "generate a try-block with the correct console call" in {
      inside(tryBlocks.headOption) {
        case Some(ctl) =>
          inside(ctl.astChildren.isCall.l) {
            case consoleCall :: Nil =>
              consoleCall.code shouldBe "Console.WriteLine(\"Hello\")"
            case _ => fail("No call node found!")
          }
        case None => fail("No `try` block found!")
      }
    }

    "generate a catch-block with a child block containing the correct console call" in {
      inside(Option(tryBlocks(1))) {
        case Some(ctl) =>
          inside(ctl.astChildren.l) {
            case (catchBlock: Block) :: Nil =>
              inside(catchBlock.astChildren.l) {
                case (excepDecl: Local) :: (consoleCall: Call) :: Nil =>
                  excepDecl.name shouldBe "e"
                  consoleCall.code shouldBe "Console.WriteLine(\"Uh, oh!\")"
                case _ => fail("Invalid `catch` block children!")
              }
            case _ => fail("No `catch` handler block found!")
          }
        case None => fail("No `catch` block found!")
      }
    }

    "generate a finally-block with an assignment of `Busy = false`" in {
      inside(tryBlocks.lastOption) {
        case Some(ctl) =>
          inside(ctl.astChildren.isCall.l) {
            case assignment :: Nil =>
              assignment.code shouldBe "Busy = false"
            case _ => fail("No call node found!")
          }
        case None => fail("No `finally` block found!")
      }
    }

  }

}
