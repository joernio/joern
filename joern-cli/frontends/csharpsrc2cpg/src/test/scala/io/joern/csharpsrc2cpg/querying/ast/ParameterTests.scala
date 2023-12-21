package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class ParameterTests extends CSharpCode2CpgFixture {

  "a default static main method" should {
    val cpg = code(basicBoilerplate(), "Program.cs")

    "generate a method node with a string[] args parameter" in {
      val x          = cpg.method.nameExact("Main").head
      val List(args) = x.parameter.l: @unchecked
      args.name shouldBe "args"
      args.typeFullName shouldBe "System.String[]"
      args.code shouldBe "string[] args"
      args.index shouldBe 1
      args.isVariadic shouldBe false
    }

  }

  "virtual method with multiple parameters" should {
    val cpg = code(
      """using System;
        |
        |namespace HelloWorld
        |{
        |  class Program
        |  {
        |    void Foo(string a, int b)
        |    {
        |      return a;
        |    }
        |  }
        |
        |}
        |""".stripMargin,
      "Program.cs"
    )

    "generate a method node with an implicit this parameter, as well as the declared parameters" in {
      val x                    = cpg.method.nameExact("Foo").head
      val List(thisNode, a, b) = x.parameter.l: @unchecked

      thisNode.name shouldBe "this"
      thisNode.typeFullName shouldBe "HelloWorld.Program"
      thisNode.code shouldBe "this"
      thisNode.index shouldBe 0
      thisNode.isVariadic shouldBe false

      a.name shouldBe "a"
      a.typeFullName shouldBe "System.String"
      a.code shouldBe "string a"
      a.index shouldBe 1
      a.isVariadic shouldBe false

      b.name shouldBe "b"
      b.typeFullName shouldBe "System.Int32"
      b.code shouldBe "int b"
      b.index shouldBe 2
      b.isVariadic shouldBe false
    }

  }

}
