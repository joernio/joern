package io.joern.php2cpg.querying

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language.*

class MemberTests extends PhpCode2CpgFixture {

  "class constants" should {
    val source = """<?php
      |class Foo {
      |  const A = 'A', B = 'B';
      |  public const C = 'C';
      |}
      |""".stripMargin

    val cpg = code(source, "foo.php").withConfig(Config().withDisableFileContent(false))

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

      inside(cpg.method.nameExact(Defines.StaticInitMethodName).l) { case List(clinitMethod) =>
        inside(clinitMethod.body.astChildren.l) { case List(self: Local, aAssign: Call, bAssign: Call, cAssign: Call) =>
          self.name shouldBe "self"
          checkConstAssign(aAssign, "A")
          checkConstAssign(bAssign, "B")
          checkConstAssign(cAssign, "C")
        }
        clinitMethod.isExternal shouldBe false
        clinitMethod.offset shouldBe Some(0)
        clinitMethod.offsetEnd shouldBe Some(source.length)
        cpg.file
          .name("foo.php")
          .content
          .map(_.substring(clinitMethod.offset.get, clinitMethod.offsetEnd.get))
          .l shouldBe List(source)
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
      inside(cpg.method.nameExact(Domain.ConstructorMethodName).l) { case List(initMethod) =>
        inside(initMethod.body.astChildren.l) { case List(aAssign: Call, bAssign: Call, cAssign: Call) =>
          checkFieldAssign(aAssign, "a")
          checkFieldAssign(bAssign, "b")
          checkFieldAssign(cAssign, "c")
        }
      }
    }
  }

  "class properties (fields) for classes with a constructor" should {

    val cpg = code("""<?php
      |class Foo {
      |  public $a = 'a', $b = 'b';
      |  final protected $c = 'c';
      |  function __construct() { }
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
      inside(cpg.method.nameExact(Domain.ConstructorMethodName).l) { case List(initMethod) =>
        inside(initMethod.body.astChildren.l) { case List(aAssign: Call, bAssign: Call, cAssign: Call) =>
          checkFieldAssign(aAssign, "a")
          checkFieldAssign(bAssign, "b")
          checkFieldAssign(cAssign, "c")
        }
      }
    }
  }

  "class const accesses should be created with the correct field access" in {
    val cpg = code("""<?php
      |Foo::X;
      |""".stripMargin)

    inside(cpg.call.nameExact(Operators.fieldAccess).l) { case List(fieldAccess) =>
      fieldAccess.code shouldBe "Foo::X"
      fieldAccess.lineNumber shouldBe Some(2)

      inside(fieldAccess.argument.l) { case List(fooArg: Identifier, xArg: FieldIdentifier) =>
        fooArg.name shouldBe "Foo"
        fooArg.code shouldBe "Foo"
        fooArg.argumentIndex shouldBe 1
        fooArg.lineNumber shouldBe Some(2)

        xArg.canonicalName shouldBe "X"
        xArg.code shouldBe "X"
        xArg.argumentIndex shouldBe 2
        xArg.lineNumber shouldBe Some(2)
      }
    }
  }

  "non-class consts" should {
    val cpg = code("""<?php
      |const X = 'X';
      |echo X;
      |""".stripMargin)

    "be treated as members of global typeDecl" in {
      inside(cpg.member.l) { case List(xMember) =>
        xMember.name shouldBe "X"
        xMember.lineNumber shouldBe Some(2)
        xMember.typeDecl.name shouldBe "<global>"

        inside(xMember.modifier.l) { case List(finalModifier) =>
          finalModifier.modifierType shouldBe ModifierTypes.FINAL
        }
      }
    }

    "be initialized in the global typedecl's <clinit> method" in {
      inside(cpg.call.nameExact(Operators.assignment).l) { case List(assign) =>
        checkConstAssign(assign, "X")
      }
    }

    "be fetched as a field access for a global field" in {
      inside(cpg.call.name("echo").argument.l) { case List(constFetch: Call) =>
        constFetch.name shouldBe Operators.fieldAccess
        constFetch.lineNumber shouldBe Some(3)
        constFetch.code shouldBe "X"

        inside(constFetch.argument.l) { case List(globalIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          globalIdentifier.name shouldBe "<global>"
          fieldIdentifier.canonicalName shouldBe "X"
        }
      }
    }
  }

  private def checkFieldAssign(assign: Call, expectedValue: String): Unit = {
    assign.name shouldBe Operators.assignment
    assign.methodFullName shouldBe Operators.assignment

    inside(assign.argument.l) { case List(targetFa: Call, source: Literal) =>
      targetFa.name shouldBe Operators.fieldAccess
      inside(targetFa.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
        identifier.name shouldBe "this"
        identifier.code shouldBe "$this"
        identifier.argumentIndex shouldBe 1

        fieldIdentifier.canonicalName shouldBe expectedValue
        fieldIdentifier.code shouldBe expectedValue
        fieldIdentifier.argumentIndex shouldBe 2
      }

      source.code shouldBe s"\"$expectedValue\""
      source.argumentIndex shouldBe 2
    }
  }

  private def checkConstAssign(assign: Call, expectedValue: String): Unit = {
    assign.name shouldBe Operators.assignment
    assign.methodFullName shouldBe Operators.assignment

    inside(assign.argument.l) { case List(target: Call, source: Literal) =>
      inside(target.argument.l) { case List(base: Identifier, field: FieldIdentifier) =>
        base.name shouldBe "self"
        field.code shouldBe expectedValue
      }

      target.name shouldBe Operators.fieldAccess
      target.code shouldBe s"self::$expectedValue"
      target.argumentIndex shouldBe 1

      source.code shouldBe s"\"$expectedValue\""
      source.argumentIndex shouldBe 2
    }
  }
}
