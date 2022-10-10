package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Modifier}
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends PhpCode2CpgFixture {

  "typedecl nodes for empty classes should have the correct basic properties set" in {
    val cpg = code("""<?php
		 |class A extends B implements C, D {}
		 |""".stripMargin)

    inside(cpg.typeDecl.nameExact("A").l) { case List(typeDecl) =>
      typeDecl.fullName shouldBe "A"
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
      fooMethod.fullName shouldBe "Foo.foo"
      fooMethod.modifier.map(_.modifierType).toSet shouldBe Set(ModifierTypes.FINAL, ModifierTypes.PUBLIC)
      fooMethod.methodReturn.typeFullName shouldBe "int"
      inside(fooMethod.parameter.l) { case List(thisParam, xParam) =>
        thisParam.name shouldBe "this"
        thisParam.code shouldBe "this"
        thisParam.dynamicTypeHintFullName should contain("Foo")
        thisParam.typeFullName shouldBe "Foo"
        thisParam.index shouldBe 0

        xParam.code shouldBe "$x"
        xParam.typeFullName shouldBe "int"
        xParam.index shouldBe 1
      }
    }
  }

  "class constants" should {
    val cpg = code("""<?php
		 |class Foo {
		 |  const A = 'A', B = 'B';
		 |  public const C = 'C';
		 |}
		 |""".stripMargin)

    "have member nodes representing them" in {
      inside(cpg.typeDecl.name("Foo").member.sortBy(_.name).toList) { case List(aMember, bMember, cMember) =>
        aMember.name shouldBe "A"
        aMember.code shouldBe "const A"

        bMember.name shouldBe "B"
        bMember.code shouldBe "const B"

        cMember.name shouldBe "C"
        cMember.code shouldBe "const C"
      }
    }

    "have an access modifier node for the C constant" in {
      inside(cpg.member("C").modifier.sortBy(_.modifierType).toList) { case List(finalModifier, publicModifier) =>
        finalModifier.modifierType shouldBe ModifierTypes.FINAL
        publicModifier.modifierType shouldBe ModifierTypes.PUBLIC
      }
    }

    "have a clinit method with the constant initializers" in {
      def checkAssign(assign: Call, expectedValue: String): Unit = {
        assign.name shouldBe Operators.assignment
        assign.methodFullName shouldBe Operators.assignment

        inside(assign.argument.l) { case List(target: Identifier, source: Literal) =>
          target.name shouldBe expectedValue
          target.code shouldBe expectedValue
          target.argumentIndex shouldBe 1

          source.code shouldBe s"\"$expectedValue\""
          source.argumentIndex shouldBe 2
        }
      }

      inside(cpg.method.nameExact(Defines.StaticInitMethodName).l) { case List(clinitMethod) =>
        inside(clinitMethod.body.astChildren.l) { case List(aAssign: Call, bAssign: Call, cAssign: Call) =>
          checkAssign(aAssign, "A")
          checkAssign(bAssign, "B")
          checkAssign(cAssign, "C")
        }
      }
    }
  }

  "class properties (fields)" should {
    val cpg = code("""<?php
                    |class Foo {
                    |  public $a = 'a', $b = 'b';
                    |  final protected $c = 'c';
                    |}
                    |""".stripMargin)

    "have member nodes representing them" in {
      inside(cpg.typeDecl.name("Foo").member.sortBy(_.name).toList) { case List(aField, bField, cField) =>
        aField.name shouldBe "a"
        aField.code shouldBe "$a"
        aField.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

        bField.name shouldBe "b"
        bField.code shouldBe "$b"
        bField.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

        cField.name shouldBe "c"
        cField.code shouldBe "$c"
        inside(cField.modifier.modifierType.l.sorted) { case List(finalModifier, protectedModifier) =>
          finalModifier shouldBe ModifierTypes.FINAL
          protectedModifier shouldBe ModifierTypes.PROTECTED
        }
      }
    }

    "have assignments added to the default constructor" in {
      def checkAssign(assign: Call, expectedValue: String): Unit = {
        assign.name shouldBe Operators.assignment
        assign.methodFullName shouldBe Operators.assignment

        inside(assign.argument.l) { case List(target: Identifier, source: Literal) =>
          target.name shouldBe expectedValue
          target.code shouldBe "$" + expectedValue
          target.argumentIndex shouldBe 1

          source.code shouldBe s"\"$expectedValue\""
          source.argumentIndex shouldBe 2
        }
      }

      inside(cpg.method.nameExact(Defines.ConstructorMethodName).l) { case List(initMethod) =>
        inside(initMethod.body.astChildren.l) { case List(aAssign: Call, bAssign: Call, cAssign: Call) =>
          checkAssign(aAssign, "a")
          checkAssign(bAssign, "b")
          checkAssign(cAssign, "c")
        }
      }
    }
  }
}
