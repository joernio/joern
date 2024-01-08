package io.joern.csharpsrc2cpg.passes.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class DeclarationTests extends CSharpCode2CpgFixture {
  "AST nodes for" should {
    val cpg = code(basicBoilerplate("""
          |int foo = 10; // When using basicBoilerplate, this is lineNumber 9 + (if any) number of global statements added
          |string bar = "bar";
          |foo + 5;
          |""".stripMargin))

    "identifiers should be created" in {
      cpg.identifier.size shouldBe 3

      val foo1: Identifier = cpg.identifier.nameExact("foo").l.head
      foo1.typeFullName shouldBe "System.Int32"
      foo1.name shouldBe "foo"
      foo1.lineNumber shouldBe Some(9)
      foo1.columnNumber shouldBe Some(4)

      val bar: Identifier = cpg.identifier.nameExact("bar").l.head
      bar.typeFullName shouldBe "System.String"
      bar.name shouldBe "bar"
      bar.lineNumber shouldBe Some(10)
      bar.columnNumber shouldBe Some(7)

      val foo2: Identifier = cpg.identifier.nameExact("foo").l.last
      foo2.typeFullName shouldBe "System.Int32"
      foo2.name shouldBe "foo"
      foo2.lineNumber shouldBe Some(11)
      foo2.columnNumber shouldBe Some(0)
    }

    "local nodes should be created" in {
      cpg.local.size shouldBe 2

      val List(foo, bar) = cpg.local.l
      foo.name shouldBe "foo"
      bar.name shouldBe "bar"

      foo.lineNumber shouldBe Some(9)
      foo.columnNumber shouldBe Some(4)

      bar.lineNumber shouldBe Some(10)
      bar.columnNumber shouldBe Some(7)

      foo.typeFullName shouldBe "System.Int32"
      bar.typeFullName shouldBe "System.String"
    }

    "literal nodes should be created" in {
      val List(fooLit, barLit, anonLit) = cpg.literal.l
      fooLit.code shouldBe "10"
      barLit.code shouldBe "\"bar\""
      anonLit.code shouldBe "5"
    }

    "identifiers should be linked to their locals via a ref edge" in {
      cpg.identifier.name("foo").refsTo.l shouldBe cpg.local.name("foo").l
    }
  }

}
