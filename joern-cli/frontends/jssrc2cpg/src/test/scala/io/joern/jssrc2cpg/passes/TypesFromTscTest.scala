package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.language._

class TypesFromTscTest extends AbstractPassTest {

  "have types for identifiers with type inference" in AstFixture("""
      |let x = "test";
      |var y = x;
      |""".stripMargin) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe Defines.STRING
  }

  "have types for identifiers from class" in AstFixture("""
      |class Foo {};
      |var y = new Foo();
      |""".stripMargin) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe "Foo"
  }

  "have types for parameters" in TsAstFixture("""
      |class Foo {};
      |let y = new Foo();
      |function bar(p1: number, p2: string) {
      |  return y;
      |}
      |""".stripMargin) { cpg =>
    val List(y1, y2) = cpg.identifier("y").l
    y1.typeFullName shouldBe "Foo"
    y2.typeFullName shouldBe "Foo"
    val List(p1) = cpg.parameter("p1").l
    p1.typeFullName shouldBe Defines.NUMBER
    val List(p2) = cpg.parameter("p2").l
    p2.typeFullName shouldBe Defines.STRING
    val List(barRet) = cpg.method("bar").methodReturn.l
    barRet.typeFullName shouldBe "(p1: number, p2: string) => Foo"
    cpg.typ.name.sorted.l shouldBe (List(
      ":program",
      io.joern.x2cpg.Defines.ConstructorMethodName,
      "Foo",
      "bar",
      "typeof Foo"
    ) ++ Defines.JSTYPES).sorted
  }

}
