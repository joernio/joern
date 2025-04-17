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
      tmpLocal.name shouldBe "foo@tmp-0"
      tmpLocal.code shouldBe "$foo@tmp-0"

      constructorBlock.lineNumber shouldBe Some(3)

      inside(constructorBlock.astChildren.l) { case List(allocAssign: Call, initCall: Call, tmpVar: Identifier) =>
        allocAssign.methodFullName shouldBe Operators.assignment
        inside(allocAssign.astChildren.l) { case List(tmpIdentifier: Identifier, allocCall: Call) =>
          tmpIdentifier.name shouldBe "foo@tmp-0"
          tmpIdentifier.code shouldBe "$foo@tmp-0"
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
          tmpIdentifier.name shouldBe "foo@tmp-0"
          tmpIdentifier.code shouldBe "$foo@tmp-0"
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

  "anonymous classes" should {
    val cpg = code("""<?php
        |new class(10) {
        |  private int $x;
        |  function __construct($x) {
        |    $this->x = $x;
        |  }
        |}
        |
        |new class(30) {
        |          private int $y;
        |          function __construct($y) {
        |            $this->y = $y;
        |          }
        |        }
        |""".stripMargin)

    "parse methods in classes correctly" in {
      inside(cpg.typeDecl.name("Test0.php:<global>@anon-class-\\d+").l) {
        case anonClass0 :: anonClass1 :: Nil =>
          val List(memberX) = anonClass0.member.l
          memberX.code shouldBe "$x"

          val List(anonConstructor0) = anonClass0.method.name("__construct").l
          anonConstructor0.fullName shouldBe "Test0.php:<global>@anon-class-0->__construct"

          val List(anonClass0ThisParam, paramX) = anonConstructor0.parameter.l
          anonClass0ThisParam.code shouldBe "this"
          paramX.code shouldBe "$x"

          inside(anonConstructor0.body.astChildren.isCall.name(Operators.assignment).l) {
            case assignmentCall :: Nil =>
              val List(lhs, rhs) = assignmentCall.argument.l
              lhs.code shouldBe "$this->x"
              rhs.code shouldBe "$x"
            case xs => fail(s"Expected assignment call, got ${xs.code.mkString("[", ",", "]")}")
          }

          val List(memberY) = anonClass1.member.l
          memberY.code shouldBe "$y"

          val List(anonConstructor1) = anonClass1.method.name("__construct").l
          anonConstructor1.fullName shouldBe "Test0.php:<global>@anon-class-1->__construct"

          val List(anonClass1ThisParam, paramY) = anonConstructor1.parameter.l
          anonClass1ThisParam.code shouldBe "this"
          paramY.code shouldBe "$y"

          inside(anonConstructor1.body.astChildren.isCall.name(Operators.assignment).l) {
            case assignmentCall :: Nil =>
              val List(lhs, rhs) = assignmentCall.argument.l
              lhs.code shouldBe "$this->y"
              rhs.code shouldBe "$y"
            case xs => fail(s"Expected assignment call, got ${xs.code.mkString("[", ",", "]")}")
          }

        case xs => fail(s"Expected two anonymous type-decls, instead got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "generate locals with correct types" in {
      inside(cpg.method.name("<global>").body.astChildren.isLocal.l) {
        case tmp0Local :: tmp1Local :: Nil =>
          tmp0Local.typeFullName shouldBe "Test0.php:<global>@anon-class-0"
          tmp0Local.name shouldBe "Test0.php:<global>@tmp-0"
          tmp1Local.typeFullName shouldBe "Test0.php:<global>@anon-class-1"
          tmp1Local.name shouldBe "Test0.php:<global>@tmp-1"
        case xs => fail(s"Expected two locals, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "generate __construct calls" in {
      inside(cpg.call.name("__construct").l) {
        case constructAnonClass0 :: constructAnonClass1 :: Nil =>
          constructAnonClass0.code shouldBe "Test0.php:<global>@anon-class-0->__construct(10)"
          val List(anonClass0Param1: Identifier, anonClass0Param2: Literal) = constructAnonClass0.argument.l: @unchecked
          anonClass0Param1.code shouldBe "$Test0.php:<global>@tmp-0"
          anonClass0Param2.code shouldBe "10"

          constructAnonClass1.code shouldBe "Test0.php:<global>@anon-class-1->__construct(30)"
          val List(anonClass1Param1: Identifier, anonClass1Param2: Literal) = constructAnonClass1.argument.l: @unchecked
          anonClass1Param1.code shouldBe "$Test0.php:<global>@tmp-1"
          anonClass1Param2.code shouldBe "30"
        case xs => fail(s"Expected two construct calls, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "generate `alloc` calls and assignments" in {
      inside(cpg.method.name("<global>").body.astChildren.isBlock.l) {
        case constructBlock1 :: constructBlock2 :: Nil =>
          inside(constructBlock1.astChildren.assignment.l) {
            case allocAssignment :: Nil =>
              val Seq(allocTarget: Identifier, allocSource: Call) =
                List(allocAssignment.target, allocAssignment.source): @unchecked
              allocTarget.code shouldBe "$Test0.php:<global>@tmp-0"
              allocSource.code shouldBe "Test0.php:<global>@anon-class-0.<alloc>()"
              allocSource.methodFullName shouldBe Operators.alloc
              allocTarget.typeFullName shouldBe "Test0.php:<global>@anon-class-0"
            case xs => fail(s"Expected one assignment, got ${xs.code.mkString("[", ",", "]")}")
          }

          inside(constructBlock2.astChildren.assignment.l) {
            case allocAssignment :: Nil =>
              val Seq(allocTarget: Identifier, allocSource: Call) =
                List(allocAssignment.target, allocAssignment.source): @unchecked
              allocTarget.code shouldBe "$Test0.php:<global>@tmp-1"
              allocSource.code shouldBe "Test0.php:<global>@anon-class-1.<alloc>()"
              allocSource.methodFullName shouldBe Operators.alloc
              allocTarget.typeFullName shouldBe "Test0.php:<global>@anon-class-1"
            case xs => fail(s"Expected some things")
          }

        case xs => fail(s"Expected one assignment, got ${xs.code.mkString("[", ",", "]")}")
      }
    }
  }

  "Anonymous class nested in class" in {
    val cpg = code("""<?php
        |class C {
        |  function D() {
        |     new class("foo") {
        |       private int $x;
        |     }
        |  }
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("C->D@anon-class-\\d+").l) {
      case anonClass :: Nil =>
        anonClass.fullName shouldBe s"C->D@anon-class-0"
        anonClass.member.code.l shouldBe List("$x")
      case xs => fail(s"Expected one anonymous class, got ${xs.code.mkString("[", ",", "]")}")
    }

    inside(cpg.method.name("D").body.astChildren.l) {
      case (localNode: Local) :: (bodyBlock: Block) :: Nil =>
        localNode.code shouldBe "$C->D@tmp-0"
        localNode.name shouldBe "C->D@tmp-0"
        localNode.typeFullName shouldBe "C->D@anon-class-0"

        inside(bodyBlock.astChildren.l) {
          case (assignmentCall: Call) :: (constructCall: Call) :: (tmpIdentifier: Identifier) :: Nil =>
            assignmentCall.methodFullName shouldBe Operators.assignment
            val List(lhs: Identifier, rhs: Call) = assignmentCall.argument.l: @unchecked
            lhs.typeFullName shouldBe "C->D@anon-class-0"
            rhs.methodFullName shouldBe Operators.alloc

            constructCall.methodFullName shouldBe "C->D@anon-class-0->__construct"

            tmpIdentifier.code shouldBe "$C->D@tmp-0"
          case xs => fail(s"Expected three children, got ${xs.code.mkString("[", ",", "]")}")
        }
      case xs => fail(s"expected localNode and body, got ${xs.code.mkString("[", ",", "]")}")
    }
  }
}
