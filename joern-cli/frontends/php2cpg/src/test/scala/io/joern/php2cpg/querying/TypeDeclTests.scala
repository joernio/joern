package io.joern.php2cpg.querying

import io.joern.php2cpg.Config
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class TypeDeclTests extends PhpCode2CpgFixture {

  "typedecl nodes for empty classes" should {
    val cpg = code("""<?php
        |class A extends B implements C, D {}
        |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "have the correct basic properties set" in {
      inside(cpg.typeDecl.nameExact("A").l) { case List(typeDecl) =>
        typeDecl.fullName shouldBe "A"
        typeDecl.lineNumber shouldBe Some(2)
        typeDecl.code shouldBe "class A extends B implements C, D"
      }
    }

    "have the content offsets set correctly" in {
      inside(cpg.typeDecl.name("A").l) { case List(typeDecl) =>
        val offsetStart = typeDecl.offset.get
        val offsetEnd   = typeDecl.offsetEnd.get
        typeDecl.file.head.content.substring(offsetStart, offsetEnd) shouldBe "class A extends B implements C, D {}"
      }
    }
  }

  "class methods" should {
    val cpg = code("""<?php
        |class Foo {
        |  final public function foo(int $x): int {
        |    return 0;
        |  }
        |}
        |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "be created correctly" in {
      inside(cpg.method.name("foo").l) { case List(fooMethod) =>
        fooMethod.fullName shouldBe s"Foo->foo"
        fooMethod.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
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

    "have the content offsets set correctly" in {
      inside(cpg.typeDecl.name("Foo").l) { case List(typeDecl) =>
        val offsetStart = typeDecl.offset.get
        val offsetEnd   = typeDecl.offsetEnd.get
        typeDecl.file.head.content.substring(offsetStart, offsetEnd) shouldBe
          """class Foo {
            |  final public function foo(int $x): int {
            |    return 0;
            |  }
            |}""".stripMargin
      }
    }
  }

  "constructors using the class name should be represented with the correct init method" in {
    val cpg = code("""<?php
        |function foo() {
        |  new Foo(42);
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(tmpLocal: Local, constructorBlock: Block) =>
      tmpLocal.name shouldBe "tmp0"
      tmpLocal.code shouldBe "$tmp0"

      constructorBlock.lineNumber shouldBe Some(3)

      inside(constructorBlock.astChildren.l) { case List(allocAssign: Call, initCall: Call, tmpVar: Identifier) =>
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

        initCall.name shouldBe "__construct"
        initCall.methodFullName shouldBe s"Foo->__construct"
        initCall.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
        initCall.code shouldBe "Foo->__construct(42)"
        inside(initCall.argument.l) { case List(tmpIdentifier: Identifier, literal: Literal) =>
          tmpIdentifier.name shouldBe "tmp0"
          tmpIdentifier.code shouldBe "$tmp0"
          tmpIdentifier.argumentIndex shouldBe 0
          tmpIdentifier._localViaRefOut should contain(tmpLocal)
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
      inside(alloc.argument(0).start.l) { case List(xIdentifier: Identifier) =>
        xIdentifier.name shouldBe "x"
        xIdentifier.code shouldBe "$x"
      }
    }
  }

  "interfaces not extending other interfaces should be created correctly" in {
    val cpg = code("""<?php
        |interface Foo {
        |  public function foo();
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("Foo").l) { case List(fooDecl) =>
      fooDecl.fullName shouldBe "Foo"
      fooDecl.code shouldBe "interface Foo"
      fooDecl.inheritsFromTypeFullName.isEmpty shouldBe true

      inside(fooDecl.astChildren.l) { case List(fooMethod: Method) =>
        fooMethod.name shouldBe "foo"
        fooMethod.fullName shouldBe s"Foo->foo"
        fooMethod.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
      }
    }
  }

  "interfaces should be able to extend multiple other interfaces" in {
    val cpg = code("""<?php
        |interface Foo extends Bar, Baz {
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("Foo").l) { case List(fooDecl) =>
      fooDecl.fullName shouldBe "Foo"
      fooDecl.code shouldBe "interface Foo extends Bar, Baz"
      fooDecl.inheritsFromTypeFullName should contain theSameElementsAs List("Bar", "Baz")
    }
  }

  "traits should have the correct code fields" in {
    val cpg = code("""<?php
        |trait Foo {
        |  public function foo() {
        |    echo "foo";
        |  }
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("Foo").l) { case List(fooDecl) =>
      fooDecl.fullName shouldBe "Foo"
      fooDecl.code shouldBe "trait Foo"
      fooDecl.inheritsFromTypeFullName.isEmpty shouldBe true

      inside(fooDecl.astChildren.l) { case List(fooMethod: Method) =>
        fooMethod.name shouldBe "foo"
        fooMethod.fullName shouldBe s"Foo->foo"
        fooMethod.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
      }
    }
  }

  "enums with cases without values should have the correct fields" in {
    val cpg = code("""<?php
        |enum Foo {
        |  case A;
        |  case B;
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("Foo").l) { case List(fooDecl) =>
      fooDecl.fullName shouldBe "Foo"
      fooDecl.code shouldBe "enum Foo"

      inside(fooDecl.astChildren.l) { case List(aMember: Member, bMember: Member) =>
        aMember.name shouldBe "A"
        aMember.code shouldBe "case A"
        aMember.lineNumber shouldBe Some(3)

        bMember.name shouldBe "B"
        bMember.code shouldBe "case B"
        bMember.lineNumber shouldBe Some(4)
      }
    }
  }

  "enums with cases with values should have the correct initializers" in {
    val cpg = code(
      """<?php
        |enum Foo {
        |  case A = "A";
        |  case B = "B";
        |}
        |""".stripMargin,
      fileName = "foo.php"
    )

    inside(cpg.typeDecl.name("Foo").l) { case List(fooDecl) =>
      fooDecl.fullName shouldBe "Foo"
      fooDecl.code shouldBe "enum Foo"

      inside(fooDecl.member.l) { case List(aMember: Member, bMember: Member) =>
        aMember.name shouldBe "A"
        aMember.code shouldBe "case A"
        aMember.lineNumber shouldBe Some(3)

        bMember.name shouldBe "B"
        bMember.code shouldBe "case B"
        bMember.lineNumber shouldBe Some(4)
      }

      inside(fooDecl.method.l) { case List(clinitMethod: Method) =>
        clinitMethod.name shouldBe Defines.StaticInitMethodName
        clinitMethod.fullName shouldBe s"Foo::${Defines.StaticInitMethodName}"
        clinitMethod.signature shouldBe "void()"
        clinitMethod.filename shouldBe "foo.php"
        clinitMethod.file.name.l shouldBe List("foo.php")

        inside(clinitMethod.body.astChildren.l) { case List(self: Local, aAssign: Call, bAssign: Call) =>
          aAssign.code shouldBe "self::A = \"A\""
          inside(aAssign.astChildren.l) { case List(aCall: Call, aLiteral: Literal) =>
            inside(aCall.argument.l) { case List(aSelf: Identifier, aField: FieldIdentifier) =>
              aSelf.name shouldBe "self"
              aField.code shouldBe "A"
            }
            aCall.name shouldBe Operators.fieldAccess
            aCall.code shouldBe "self::A"

            aLiteral.code shouldBe "\"A\""
          }

          bAssign.code shouldBe "self::B = \"B\""
          inside(bAssign.astChildren.l) { case List(bCall: Call, bLiteral: Literal) =>
            inside(bCall.argument.l) { case List(bSelf: Identifier, bField: FieldIdentifier) =>
              bSelf.name shouldBe "self"
              bField.code shouldBe "B"
            }
            bCall.name shouldBe Operators.fieldAccess
            bCall.code shouldBe "self::B"

            bLiteral.code shouldBe "\"B\""
          }
        }
      }
    }
  }

  "the global type decl should have the correct name" in {
    val cpg = code("<?php echo 0;", fileName = "foo.php")

    cpg.typeDecl.nameExact("<global>").fullName.l shouldBe List("foo.php:<global>")
  }

  "class magic constants" when {
    "called on a class name should give the fully qualified class name" in {
      val cpg = code("""<?php
        |namespace foo;
        |class Foo {}
        |
        |function test() {
        |  Foo::class;
        |}
        |""")

      inside(cpg.method.name("test").body.astChildren.l) { case List(fooRef: TypeRef) =>
        fooRef.typeFullName shouldBe "foo\\Foo"
        fooRef.lineNumber shouldBe Some(6)
      }
    }

    "called on an object should give the fully qualified type name" in {
      val cpg = code("""<?php
        |namespace foo;
        |class Foo {}
        |
        |function test(Foo $f) {
        |  $f::class;
        |}
        |""")

      inside(cpg.method.name("test").body.astChildren.l) { case List(fooRef: TypeRef) =>
        // TODO The typeFullName here is missing, even though we should get it. Fix with types in general.
        // fooRef.typeFullName shouldBe "foo\\Foo"
        fooRef.lineNumber shouldBe Some(6)
      }
    }
  }

  "static/const member of class should be put in <clinit> method" in {
    val cpg = code("""<?php
        |class Foo {
        |  static $A = "A";
        |  const B = "B";
        |}
        |""")

    inside(cpg.method.nameExact(Defines.StaticInitMethodName).l) { case List(clinitMethod) =>
      inside(clinitMethod.body.astChildren.l) { case List(self: Local, bAssign: Call, aAssign: Call) =>
        self.name shouldBe "self"
        inside(aAssign.astChildren.l) { case List(aCall: Call, aLiteral: Literal) =>
          inside(aCall.argument.l) { case List(aSelf: Identifier, aField: FieldIdentifier) =>
            aSelf.name shouldBe "self"
            aField.code shouldBe "A"
          }
          aCall.name shouldBe Operators.fieldAccess
          aCall.code shouldBe "self::$A"

          aLiteral.code shouldBe "\"A\""
        }

        inside(bAssign.astChildren.l) { case List(bCall: Call, bLiteral: Literal) =>
          inside(bCall.argument.l) { case List(bSelf: Identifier, bField: FieldIdentifier) =>
            bSelf.name shouldBe "self"
            bField.code shouldBe "B"
          }
          bCall.name shouldBe Operators.fieldAccess
          bCall.code shouldBe "self::B" // Notice there is no `$` in front of the const member

          bLiteral.code shouldBe "\"B\""
        }
      }
    }
  }
}
