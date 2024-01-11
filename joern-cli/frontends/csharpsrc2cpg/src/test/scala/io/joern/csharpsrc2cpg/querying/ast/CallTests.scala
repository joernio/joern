package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class CallTests extends CSharpCode2CpgFixture {

  "builtin calls" should {

    val cpg = code(basicBoilerplate())

    "create a call node with arguments" in {
      val writeLine = cpg.call.nameExact("WriteLine").headOption match
        case Some(callNode) => callNode
        case None           => fail("Node not found!")

      writeLine.name shouldBe "WriteLine"
      // writeLine.methodFullName shouldBe "System.Console.WriteLine" TODO: Handle when call graph is being done
      // writeLine.typeFullName shouldBe "void"
      writeLine.code shouldBe "Console.WriteLine(\"Hello, world!\")"

      inside(writeLine.argument.l) {
        case (base: Identifier) :: (strArg: Literal) :: Nil =>
//          base.typeFullName shouldBe "System.Console"
          base.name shouldBe "Console"
          base.code shouldBe "Console"
          base.argumentIndex shouldBe 0

          strArg.typeFullName shouldBe "System.String"
          strArg.code shouldBe "\"Hello, world!\""
          strArg.argumentIndex shouldBe 1
        case _ => fail("Arguments malformed or not found!")
      }
    }

    "be resolve a method full name without the definition clearly defined" in {}

  }

}
