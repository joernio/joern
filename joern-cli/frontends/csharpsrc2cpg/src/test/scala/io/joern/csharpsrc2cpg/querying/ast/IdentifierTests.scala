package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IdentifierTests extends CSharpCode2CpgFixture {
  "identifier tests" should {
    val cpg = code(basicBoilerplate("""
        |const int bar = 10;
        |const string fooBar = "fooBar";
        |const bool isTrue = true;
        |""".stripMargin))
    "create identifier nodes created using const" in {
      println(cpg.method.nameExact("Main").astChildren.isModifier.code.l)
      inside(cpg.method.nameExact("Main").astChildren.isIdentifier.l) {
        case bar :: fooBar :: isTrue :: Nil =>
        case _                              => fail("Exactly 3 identifiers expected inside `Foo`.")
      }
    }
  }
}
