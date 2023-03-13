package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class TypeRecoveryPassTests extends DataFlowCodeToCpgSuite {

  "literals declared from built-in types" should {
    lazy val cpg = code("""
        |let x = 123;
        |
        |function foo_shadowing() {
        |   let x = "foo";
        |}
        |
        |z = {'a': 123};
        |z = [1, 2, 3];
        |
        |z.push(4)
        |""".stripMargin)

    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__ecma.String", "__ecma.Number")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__ecma.String", "__ecma.Number")
    }

    "resolve 'z' types correctly" in {
      // The dictionary/object type is just considered "ANY" which is fine for now
      cpg.identifier("z").typeFullName.toSet.headOption shouldBe Some("__ecma.Array")
    }

    "resolve 'z' identifier call correctly" in {
      val List(zAppend) = cpg.call("push").l
      zAppend.methodFullName shouldBe "__ecma.Array.push"
    }
  }

}
