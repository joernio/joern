package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Method, MethodRef, TypeDecl}
import io.shiftleft.semanticcpg.language.*
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap

class LambdaTests extends CSharpCode2CpgFixture {

  "a simple lambda used and defined in a high-order function" should {

    val cpg = code(basicBoilerplate("""
        |int[] numbers = { 2, 3, 4, 5 };
        |var squaredNumbers = numbers.Select(x => x * x);
        |""".stripMargin))

    "create an anonymous method declaration" in {
      inside(cpg.method("Main").astChildren.collectAll[Method].l) { case anon :: Nil =>
        anon.name shouldBe "<lambda>0"
        anon.fullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"

        inside(anon.parameter.l) { case x :: Nil =>
          x.name shouldBe "x"
          x.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
          x.index shouldBe 1
        }

      }
    }

    "create an anonymous type declaration" in {
      inside(cpg.method("Main").astChildren.collectAll[TypeDecl].l) { case anon :: Nil =>
        anon.name shouldBe "<lambda>0"
        anon.fullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"
      }
    }

    "pass a method reference to the anonymous function in the call `Select`" in {
      inside(cpg.call("Select").argument.l) { case (numbers: Identifier) :: (closure: MethodRef) :: Nil =>
        numbers.name shouldBe "numbers"
        numbers.typeFullName shouldBe s"${DotNetTypeMap(BuiltinTypes.Int)}[]"

        closure.methodFullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"
        closure.referencedMethod.name shouldBe "<lambda>0"
      }
    }

  }

  "a paranthesized lambda expression" should {
    val cpg = code(basicBoilerplate("""
        |int[] numbers = { 2, 3, 4, 5 };
        |var squaredNumbers = numbers.Select((x,y) => {
        |   Console.WriteLine(x);
        |   x * x;
        |});
        |""".stripMargin))

    "create an anonymous method declaration" in {
      inside(cpg.method("Main").astChildren.collectAll[Method].l) { case anon :: Nil =>
        anon.name shouldBe "<lambda>0"
        anon.fullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"

        inside(anon.parameter.l) { case x :: y :: Nil =>
          x.name shouldBe "x"
          x.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
          x.index shouldBe 1

          y.name shouldBe "y"
          y.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
          y.index shouldBe 2
        }

      }
    }

    "create an anonymous type declaration" in {
      inside(cpg.method("Main").astChildren.collectAll[TypeDecl].l) { case anon :: Nil =>
        anon.name shouldBe "<lambda>0"
        anon.fullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"
      }
    }

    "pass a method reference to the anonymous function in the call `Select`" in {
      inside(cpg.call("Select").argument.l) { case (numbers: Identifier) :: (closure: MethodRef) :: Nil =>
        numbers.name shouldBe "numbers"
        numbers.typeFullName shouldBe s"${DotNetTypeMap(BuiltinTypes.Int)}[]"

        closure.methodFullName shouldBe "HelloWorld.Program.Main.<lambda>0:<unresolvedSignature>"
        closure.referencedMethod.name shouldBe "<lambda>0"
      }
    }

    "have correct attributes for it's body" in {
      inside(cpg.method("Main").astChildren.collectAll[Method].l) { case anon :: Nil =>
        inside(anon.ast.collectAll[Call].nameExact("WriteLine").l) { case callNode :: Nil =>
          callNode.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.Int32)"
        }
      }
    }
  }

}
