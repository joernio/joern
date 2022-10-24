package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local}
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

  "constructors using the class name should be represented with the correct init method" in {
    val cpg = code("""<?php
        |function foo() {
        |  new Foo(42);
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.isBlock.l) { case List(constructorBlock) =>
      constructorBlock.lineNumber shouldBe Some(3)

      inside(constructorBlock.astChildren.l) {
        case List(tmpLocal: Local, allocAssign: Call, initCall: Call, tmpVar: Identifier) =>
          tmpLocal.name shouldBe "tmp0"
          tmpLocal.code shouldBe "$tmp0"

          allocAssign.methodFullName shouldBe Operators.assignment
          inside(allocAssign.astChildren.l) { case List(tmpIdentifier: Identifier, allocCall: Call) =>
            tmpIdentifier.name shouldBe "tmp0"
            tmpIdentifier.code shouldBe "$tmp0"
            tmpIdentifier._localViaRefOut should contain(tmpLocal)

            allocCall.name shouldBe Operators.alloc
            allocCall.methodFullName shouldBe Operators.alloc
            allocCall.lineNumber shouldBe Some(3)
            allocCall.code shouldBe "Foo.<alloc>()"
          }

          initCall.name shouldBe "<init>"
          initCall.methodFullName shouldBe s"Foo.<init>:${Defines.UnresolvedSignature}(1)"
          initCall.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
          initCall.code shouldBe "Foo.<init>(42)"
          inside(initCall.receiver.l) { case List(tmpIdentifier: Identifier) =>
            tmpIdentifier.name shouldBe "tmp0"
            tmpIdentifier.code shouldBe "$tmp0"
            tmpIdentifier.argumentIndex shouldBe 0
            tmpIdentifier._localViaRefOut should contain(tmpLocal)
          }
          inside(initCall.argument.l) { case List(literal: Literal) =>
            literal.code shouldBe "42"
            literal.argumentIndex shouldBe 1
          }
      }
    }
  }

  "constructors using expressions for the class name should have the correct alloc receiver" in {
    val cpg = code("""<?php
        |function foo() {
        |  new $x();
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.alloc).l) { case List(alloc: Call) =>
      alloc.name shouldBe Operators.alloc
      alloc.methodFullName shouldBe Operators.alloc
      alloc.code shouldBe "$x.<alloc>()"
      inside(alloc.receiver.l) { case List(xIdentifier: Identifier) =>
        xIdentifier.name shouldBe "x"
        xIdentifier.code shouldBe "$x"
      }
    }
  }
}
