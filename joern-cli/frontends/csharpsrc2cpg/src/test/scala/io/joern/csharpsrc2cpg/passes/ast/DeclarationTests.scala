package io.joern.csharpsrc2cpg.passes.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DeclarationTests extends CSharpCode2CpgFixture {
  "AST nodes for" should {
    val cpg = code(
      basicBoilerplate(
        """
          |int foo = 10; // When using basicBoilerplate, this is lineNumber 9 + (if any) number of global statements added
          |string bar = "bar";
          |foo + 5;
          |""".stripMargin,
        globalDeclarations = """
            |int baz=1;""".stripMargin
      )
    )

    "identifiers should be created" in {
      cpg.identifier.size shouldBe 4

      cpg.identifier.typeFullName.l shouldBe List("System.Int32", "System.Int32", "System.String", "System.Int32")

      cpg.identifier.name.l shouldBe List("baz", "foo", "bar", "foo")

      cpg.identifier.name("foo").lineNumber.l shouldBe List(10, 12)
      cpg.identifier.name("foo").columnNumber.l shouldBe List(4, 0)

      cpg.identifier.name("bar").lineNumber.l shouldBe List(11)
      cpg.identifier.name("bar").columnNumber.l shouldBe List(7)
    }

    "local nodes should be created" in {
      cpg.local.size shouldBe 3
      cpg.local.name.l shouldBe List("baz", "foo", "bar")

      cpg.local.name("foo").lineNumber.l shouldBe List(10)
      cpg.local.name("foo").columnNumber.l shouldBe List(4)

      cpg.local.typeFullName.l shouldBe List("System.Int32", "System.Int32", "System.String")
    }

    "literal nodes should be created" in {
      cpg.literal.code.l shouldBe List("1", "10", "\"bar\"", "5")
    }

    "identifiers should be linked to their locals via a ref edge" in {
      cpg.identifier.name("foo").refsTo.l shouldBe cpg.local.name("foo").l
    }
  }

}
