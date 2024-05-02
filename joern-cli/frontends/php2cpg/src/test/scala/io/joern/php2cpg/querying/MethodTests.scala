package io.joern.php2cpg.querying

import io.joern.php2cpg.Config
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

class MethodTests extends PhpCode2CpgFixture {

  "method nodes should be created with the correct fields" in {
    val cpg = code(
      """<?php
     |function foo(): int {}
     |""".stripMargin,
      fileName = "foo.php"
    )

    inside(cpg.method.name("foo").l) { case List(fooMethod) =>
      fooMethod.fullName shouldBe "foo"
      fooMethod.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
      fooMethod.lineNumber shouldBe Some(2)
      fooMethod.code shouldBe "function foo()"
      fooMethod.astParentType shouldBe "METHOD"
      fooMethod.astParentFullName.endsWith("<global>") shouldBe true

      inside(fooMethod.methodReturn.start.l) { case List(methodReturn) =>
        methodReturn.typeFullName shouldBe "int"
        methodReturn.code shouldBe "RET"
        methodReturn.lineNumber shouldBe Some(2)
      }
    }
  }

  "static variables without default values" should {
    val cpg = code("""<?php
        |function foo() {
        |  static $x, $y;
        |}
        |""".stripMargin)

    "not leave orphan identifiers" in {
      cpg.identifier.filter(identifier => Try(identifier.astParent.isEmpty).getOrElse(true)).toList shouldBe Nil
    }

    "be represented as the correct local nodes" in {
      inside(cpg.method.name("foo").body.astChildren.l) { case List(xLocal: Local, yLocal: Local) =>
        xLocal.name shouldBe "x"
        xLocal.code shouldBe "static $x"
        xLocal.lineNumber shouldBe Some(3)

        yLocal.name shouldBe "y"
        yLocal.code shouldBe "static $y"
        yLocal.lineNumber shouldBe Some(3)
      }
    }

  }

  "static variables with default values should have the correct initialisers" in {
    val cpg = code("""<?php
        |function foo() {
        |  static $x = 42, $y;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(xLocal: Local, xAssign: Call, yLocal: Local) =>
      xLocal.name shouldBe "x"
      xLocal.code shouldBe "static $x"
      xLocal.lineNumber shouldBe Some(3)

      yLocal.name shouldBe "y"
      yLocal.code shouldBe "static $y"
      yLocal.lineNumber shouldBe Some(3)

      xAssign.name shouldBe Operators.assignment
      xAssign.code shouldBe "static $x = 42"
      inside(xAssign.argument.l) { case List(xIdent: Identifier, literal: Literal) =>
        xIdent.name shouldBe "x"
        xIdent.code shouldBe "$x"
        xIdent.lineNumber shouldBe Some(3)

        literal.code shouldBe "42"
        literal.lineNumber shouldBe Some(3)
      }
    }
  }

  "methods should be accessible from the file node" in {
    val cpg = code(
      """<?php
        |function foo() {
        |  static $x = 42, $y;
        |}
        |""".stripMargin,
      fileName = "test.php"
    )

    cpg.file.method.name.toSet shouldBe Set("<global>", "foo")
    cpg.method.name("foo").filename.l shouldBe List("test.php")
  }

  "global method full name should include the file for uniqueness" in {
    val cpg = code("<?php", fileName = "test.php")
    cpg.method.nameExact("<global>").fullName.l shouldBe List("test.php:<global>")
  }

  "methods with non-unicode-legal characters" should {
    val cpg = code("""<?php
        |function foo() {
        |  $x = "\xFF";
        |}
        |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "be created with escaped char codes" in {
      cpg.file.method.name.toSet shouldBe Set("<global>", "foo")
      cpg.assignment.code.l shouldBe List("$x = \"\\\\xFF\"")
    }

    "set the file content correctly" in {
      inside(cpg.method.nameExact("foo").l) { case List(fooMethod) =>
        val offsetStart = fooMethod.offset.get
        val offsetEnd   = fooMethod.offsetEnd.get
        fooMethod.file.head.content.substring(offsetStart, offsetEnd) shouldBe
          """function foo() {
            |  $x = "\xFF";
            |}""".stripMargin
      }
    }
  }

  "methods with unicode characters in source" should {
    val cpg = code("""<?php
        |function foo() {
        |  // ⦝
        |  $x = "🙂⨌🙂𐇐🙂🙂🙂🙂";
        |}""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "set the content field correctly" in {
      inside(cpg.method.nameExact("foo").l) { case List(fooMethod) =>
        val offsetStart = fooMethod.offset.get
        val offsetEnd   = fooMethod.offsetEnd.get
        fooMethod.file.head.content.substring(offsetStart, offsetEnd) shouldBe
          """function foo() {
            |  // ⦝
            |  $x = "🙂⨌🙂𐇐🙂🙂🙂🙂";
            |}""".stripMargin
      }
    }
  }

  "explicit constructors" should {
    val cpg = code(
      """<?php
        |class Foo {
        |  function __construct() {}
        |}
        |""".stripMargin,
      fileName = "foo.php"
    ).withConfig(Config().withDisableFileContent(false))

    "have the constructor modifier set" in {
      inside(cpg.method.nameExact("__construct").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.CONSTRUCTOR, ModifierTypes.PUBLIC)
      }

      cpg.method.name("__construct").size shouldBe cpg.method.isConstructor.size
    }

    "have a filename set with traversal to the file" in {
      inside(cpg.method.nameExact("__construct").l) { case List(constructor) =>
        constructor.filename shouldBe "foo.php"
        constructor.file.name.l shouldBe List("foo.php")
      }
    }

    "have the content offsets set correctly" in {
      inside(cpg.method.nameExact("__construct").l) { case List(constructor) =>
        val offsetStart = constructor.offset.get
        val offsetEnd   = constructor.offsetEnd.get
        constructor.file.head.content.substring(offsetStart, offsetEnd) shouldBe "function __construct() {}"
      }
    }
  }

  "default constructors" should {
    val cpg = code(
      """<?php
        |class Foo { }
        |""".stripMargin,
      fileName = "foo.php"
    )

    "have the constructor modifier set" in {
      inside(cpg.method.nameExact("__construct").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(
          ModifierTypes.CONSTRUCTOR,
          ModifierTypes.PUBLIC,
          ModifierTypes.VIRTUAL
        )
      }

      cpg.method.name("__construct").size shouldBe cpg.method.isConstructor.size
    }

    "have a filename set with traversal to the file" in {
      inside(cpg.method.nameExact("__construct").l) { case List(constructor) =>
        constructor.filename shouldBe "foo.php"
        constructor.file.name.l shouldBe List("foo.php")
      }
    }
  }

}
