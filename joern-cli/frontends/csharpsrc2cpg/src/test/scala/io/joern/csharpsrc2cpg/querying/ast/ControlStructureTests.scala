package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.Block
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.JumpTarget
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
              exp.typeFullName shouldBe "System.Exception"
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

    val tryElements = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.TRY).astChildren.l

    "generate a try control structure with three children correctly" in {
      val List(tryBlock) = tryElements.isBlock.l
      tryBlock.astChildren.isCall.code.l shouldBe List("""Console.WriteLine("Hello")""")
      val List(catchBlock) = tryElements.isControlStructure.isCatch.astChildren.l
      catchBlock.astChildren.isCall.code.l shouldBe List("""Console.WriteLine("Uh, oh!")""")
      val List(finallyBlock) = tryElements.isControlStructure.isFinally.astChildren.l
      finallyBlock.astChildren.isCall.code.l shouldBe List("""Busy = false""")
    }
  }

  "the switch statement" should {
    val cpg = code(basicBoilerplate("""
        |switch (i) {
        | case > 0:
        |   i++;
        |   break;
        | case < 0:
        |   i--;
        |   break;
        | default:
        |  i += 10;
        |  break;
        |}
        |""".stripMargin))

    "create a control structure node and contain correct astChildren" in {
      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.SWITCH).l) {
        case switchNode :: Nil =>
          switchNode.code shouldBe "switch (i)";
          switchNode.controlStructureType shouldBe ControlStructureTypes.SWITCH

          inside(switchNode.astChildren.isBlock.l) { case case1 :: case2 :: case3 :: Nil =>
            val List(incCall) = case1.astChildren.isCall.l;
            incCall.code shouldBe "i++"

            val List(decCall) = case2.astChildren.isCall.l;
            decCall.code shouldBe "i--"

            val List(plusEqualsCall) = case3.astChildren.isCall.l;
            plusEqualsCall.code shouldBe "i += 10"
          }

          inside(switchNode.astChildren.collect { case j: JumpTarget => j }.l) {
            case case1 :: case2 :: defaultCase :: Nil =>
              case1.code shouldBe "case > 0:"
              case2.code shouldBe "case < 0:"
              defaultCase.code shouldBe "default:"
          }
      }
    }
  }

  "switch statement with multiple labels" should {
    val cpg = code(basicBoilerplate("""
        |switch (i) {
        | case > 0:
        | case < 10:
        |   i++;
        |   break;
        | case 10:
        |   i--;
        |   break;
        | default:
        |  i += 10;
        |  break;
        |}
        |""".stripMargin))

    "create a control structure node with correct label and case clauses" in {

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.SWITCH).l) {
        case switchNode :: Nil =>
          switchNode.code shouldBe "switch (i)";
          switchNode.controlStructureType shouldBe ControlStructureTypes.SWITCH

          inside(switchNode.astChildren.collect { case j: JumpTarget => j }.l) {
            case case1 :: case1_1 :: case2 :: defaultCase :: Nil =>
              case1.code shouldBe "case > 0:"
              case2.code shouldBe "case 10:"
              case1_1.code shouldBe "case < 10:"
              defaultCase.code shouldBe "default:"
          }
      }
    }

  }

  "a using statement" should {
    val cpg = code(basicBoilerplate("""
        |var numbers = new List<int>();
        |using (StreamReader reader = File.OpenText("numbers.txt"))
        |{
        |    string line;
        |    while ((line = reader.ReadLine()) is not null)
        |    {
        |        if (int.TryParse(line, out int number))
        |        {
        |            numbers.Add(number);
        |        }
        |    }
        |}
        |""".stripMargin))

    val tryElements = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.TRY).astChildren.l

    "generate a try control structure with two children correctly" in {
      val List(tryBlock) = tryElements.isBlock.l
      tryBlock.code shouldBe "try"
      val List(finallyBlock) = tryElements.isControlStructure.isFinally.astChildren.l
      val List(disposeCall)  = finallyBlock.astChildren.isCall.l
      disposeCall.code shouldBe "reader.Dispose()"
      disposeCall.name shouldBe "Dispose"
      disposeCall.methodFullName shouldBe "System.Disposable.Dispose:System.Void()"
    }

  }

  "a variable defined within a using statement" should {
    val cpg = code("""
        |namespace other
        |{
        |    public class General
        |    {
        |        public static void Call(string name)
        |        {
        |            using (SqlConnection connection = new SqlConnection(name))
        |            {
        |                try
        |                {
        |                    connection.Open();
        |                }
        |                catch (Exception ex)
        |                {
        |                    Console.WriteLine(ex.Message);
        |                    connection.Close();
        |                }
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    "partially resolve calls on the defined variable" in {
      inside(cpg.call.name("Open").methodFullName.l) {
        case x :: Nil => x shouldBe "SqlConnection.Open:<unresolvedSignature>"
        case _        => fail("Unexpected call node structure")
      }
    }
  }

}
