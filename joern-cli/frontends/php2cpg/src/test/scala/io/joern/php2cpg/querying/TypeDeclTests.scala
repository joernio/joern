package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends PhpCode2CpgFixture {

  "typedecl nodes for empty classes should have the correct basic properties set" in {
    val cpg = code("""<?php
		 |class A extends B implements C, D {}
		 |""".stripMargin)

    inside(cpg.typeDecl.nameExact("A").l) { case List(typeDecl) =>
      typeDecl.fullName shouldBe "<global>.A"
      typeDecl.lineNumber shouldBe Some(2)
      typeDecl.code shouldBe "class A extends B implements C, D"
    }
  }

  "class methods should be created correctly" in {
    val cpg = code("""<?php
		 |class Foo {
		 |  final public function foo(int $x): int {
		 |    return 0;
		 |  }
		 |}
		 |""".stripMargin)

    inside(cpg.method.name("foo").l) { case List(fooMethod) =>
      fooMethod.fullName shouldBe "<global>.Foo.foo"
      fooMethod.modifier.map(_.modifierType).toSet shouldBe Set(ModifierTypes.FINAL, ModifierTypes.PUBLIC)
      fooMethod.methodReturn.typeFullName shouldBe "int"
      inside(fooMethod.parameter.l) { case List(thisParam, xParam) =>
        thisParam.name shouldBe "this"
        thisParam.code shouldBe "this"
        thisParam.dynamicTypeHintFullName should contain("<global>.Foo")
        thisParam.typeFullName shouldBe "<global>.Foo"
        thisParam.index shouldBe 0

        xParam.code shouldBe "$x"
        xParam.typeFullName shouldBe "int"
        xParam.index shouldBe 1
      }
    }
  }

}
