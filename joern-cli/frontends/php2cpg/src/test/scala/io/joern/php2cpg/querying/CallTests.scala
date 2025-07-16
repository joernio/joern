package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.parser.Domain
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class CallTests extends PhpCode2CpgFixture {

  "variable call arguments with names matching methods should not have a methodref" in {
    val cpg = code("""<?php
      |$a = file("ABC");
      |$file = $a->contents();
      |foo($file);
      |""".stripMargin)

    val fileCall = cpg.call.nameExact("file").head
    fileCall.code shouldBe "file(\"ABC\")"
    fileCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    inside(fileCall.astChildren.l) { case (arg: Literal) :: Nil =>
      arg.code shouldBe "\"ABC\""
      arg.argumentIndex shouldBe 1
    }

    val contentsCall = cpg.call.nameExact("contents").head
    contentsCall.code shouldBe "$a->contents()"
    contentsCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    inside(contentsCall.astChildren.l) { case (receiver: Identifier) :: Nil =>
      receiver.name shouldBe "a"
      receiver.code shouldBe "$a"
      receiver.argumentIndex shouldBe 0
    }

    val fooCall = cpg.call.name("foo").head
    fooCall.name shouldBe "foo"
    fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    inside(cpg.call.name("foo").argument.l) { case List(fileArg: Identifier) =>
      fileArg.name shouldBe "file"
    }
  }

  "halt_compiler calls should be created correctly" in {
    val cpg = code("""<?php
        |__halt_compiler();
        |""".stripMargin)

    inside(cpg.call.l) { case List(haltCompiler) =>
      haltCompiler.name shouldBe NameConstants.HaltCompiler
      haltCompiler.methodFullName shouldBe NameConstants.HaltCompiler
      haltCompiler.code shouldBe s"${NameConstants.HaltCompiler}()"
      haltCompiler.lineNumber shouldBe Some(2)
      haltCompiler.astChildren.size shouldBe 0
    }
  }

  "inline HTML should be represented as an echo statement" in {
    val cpg = code("TEST CODE PLEASE IGNORE")

    inside(cpg.call.name(".*echo").l) { case List(echoCall) =>
      echoCall.name shouldBe "echo"
      echoCall.argument.code.l shouldBe List("\"TEST CODE PLEASE IGNORE\"")
    }
  }

  "function calls with simple names should be correct" in {
    val cpg = code("""<?php
        |foo($x);
        |""".stripMargin)

    inside(cpg.call.l) { case List(fooCall) =>
      fooCall.name shouldBe "foo"
      fooCall.methodFullName shouldBe s"foo"
      fooCall.signature shouldBe ""
      fooCall.receiver.isEmpty shouldBe true
      fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      fooCall.lineNumber shouldBe Some(2)
      fooCall.code shouldBe "foo($x)"

      inside(fooCall.argument.l) { case List(xArg: Identifier) =>
        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "static method calls with simple names" should {
    val cpg = code("""<?php
        |Foo::foo($x);
        |""".stripMargin)

    "have the correct method node defined" in {
      inside(cpg.call.l) { case List(fooCall) =>
        fooCall.name shouldBe "foo"
        fooCall.methodFullName shouldBe s"Foo${Domain.MetaTypeDeclExtension}.foo"
        fooCall.receiver.isEmpty shouldBe true
        fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fooCall.lineNumber shouldBe Some(2)
        fooCall.code shouldBe "Foo::foo($x)"
      }
    }

    "have the correct arguments" in {
      inside(cpg.call.argument.l) { case List(xArg: Identifier) =>
        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }

    "have the correct child nodes" in {
      inside(cpg.call.astChildren.l) { case List(arg: Identifier) =>
        arg.name shouldBe "x"
      }
    }

    "not create an identifier for the class target" in {
      inside(cpg.identifier.l) { case List(xArg) =>
        xArg.name shouldBe "x"
      }
    }
  }

  /* This possibly should exist in NamespaceTests.scala */
  "static method calls that refer to self" should {
    val cpg = code("""
        |<?php
        |class ClassA {
        |   function foo($x) {
        |     return self::bar($x);
        |   }
        |   static function bar($param) {
        |     return 0;
        |   }
        |}
        |""".stripMargin)

    "resolve the correct method full name" in {
      val List(barCall) = cpg.call("bar").take(1).l
      barCall.name shouldBe "bar"
      barCall.methodFullName shouldBe s"ClassA${Domain.MetaTypeDeclExtension}.bar"
      barCall.receiver.isEmpty shouldBe true
      barCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      barCall.code shouldBe "self::bar($x)"
    }
  }

  "method calls with simple names should be correct" in {
    val cpg = code("""<?php
        |$f->foo($x);
        |""".stripMargin)

    inside(cpg.call.nameExact("foo").l) { case List(fooCall) =>
      fooCall.name shouldBe "foo"
      fooCall.methodFullName shouldBe """<unresolvedNamespace>.foo"""
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      fooCall.lineNumber shouldBe Some(2)
      fooCall.code shouldBe "$f->foo($x)"

      inside(fooCall.argument.l) { case List(fRecv: Identifier, xArg: Identifier) =>
        fRecv.name shouldBe "f"
        fRecv.code shouldBe "$f"
        fRecv.lineNumber shouldBe Some(2)

        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "method calls with complex names should be correct" in {
    val cpg = code("""<?php
        |$$f->{$foo}($x);
        |""".stripMargin)

    inside(cpg.call.filter(_.name != Operators.fieldAccess).l) { case List(fooCall) =>
      fooCall.name shouldBe "$foo"
      fooCall.methodFullName shouldBe """<unresolvedNamespace>.$foo"""
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      fooCall.lineNumber shouldBe Some(2)
      fooCall.code shouldBe "$$f->$foo($x)"

      inside(fooCall.argument.l) { case List(fRecv: Identifier, xArg: Identifier) =>
        fRecv.name shouldBe "f"
        fRecv.code shouldBe "$$f"
        fRecv.lineNumber shouldBe Some(2)

        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "the code field of Call with array unpack should be correct" in {
    val cpg = code("""
        |<?php
        |function test(...$param) { echo $param; }
        |$args = [1, 2, 3];
        |test(...$args);
        |""".stripMargin)
    val call = cpg.call("test").head
    call.code shouldBe "test(...$args)"
  }

  "static call in a static function to a static function" in {
    val cpg = code("""<?php
        |class Foo {
        |  static function foo() {
        |    self::bar();
        |
        |    new class(10) {
        |       function foz() {
        |         self::boz();
        |       }
        |
        |       static function boz() {}
        |    }
        |  }
        |
        |  private static function bar() {}
        |}
        |
        |Foo::bar();
        |""".stripMargin)

    cpg.method.name("foo").call.name("bar").methodFullName.l shouldBe List(s"Foo${Domain.MetaTypeDeclExtension}.bar")
    cpg.method.name("foz").call.name("boz").methodFullName.l shouldBe List(
      s"Foo<metaclass>.foo.anon-class-0${Domain.MetaTypeDeclExtension}.boz"
    )
  }

  "a static call in a namespace should have a full name including the namespace path" in {
    val cpg = code("""<?php
        |
        |namespace Foo\Bar {
        |
        | function baz() {}
        |
        | baz()
        |
        |}
        |
        |>
        |""".stripMargin)

    cpg.call.nameExact("baz").methodFullName.l shouldBe List(s"Foo\\Bar\\baz")
  }

  "static call in a dynamic function" in {
    val cpg = code("""<?php
         |class Foo {
         |  function foo() {
         |    self::bar();
         |
         |    new class(10) {
         |       function foz() {
         |         self::boz();
         |       }
         |
         |       static function boz() {}
         |    }
         |  }
         |
         |  private static function bar() {}
         |}
         |""".stripMargin)
    cpg.method.name("foz").call.name("boz").methodFullName.l shouldBe List(
      s"Foo.foo.anon-class-0${Domain.MetaTypeDeclExtension}.boz"
    )
  }

  "a chained call from an external namespace should have normalized '.' method delimiters" in {
    val cpg = code("""
        |<?php
        |use Foo\Bar\Http;
        |
        |Http::retry(3)->timeout(10);
        |""".stripMargin)

    cpg.call("retry").methodFullName.head shouldBe "Foo\\Bar\\Http<metaclass>.retry"
    cpg.call("timeout").methodFullName.head shouldBe s"${Defines.UnresolvedNamespace}.timeout"
  }

  "chained calls should alias calls in receivers and bases" in {
    val cpg = code("""
        |<?php
        |function test($obj) {
        | return $obj->foo()->bar();
        |}
        |""".stripMargin)

    // These calls should only happen once, as per the code
    cpg.call("foo").size shouldBe 1
    cpg.call("bar").size shouldBe 1

    val foo = cpg.call("foo").head
    foo.code shouldBe "$obj->foo()"
    inside(cpg.call("foo").astChildren.l) { case (fa: Identifier) :: Nil =>
      fa.name shouldBe "obj"
      fa.code shouldBe "$obj"
    }

    val bar = cpg.call("bar").head
    bar.code shouldBe "$obj->foo()->bar()"
    inside(cpg.call("bar").astChildren.l) { case (fa: Call) :: Nil =>
      fa.name shouldBe "foo"
      fa.code shouldBe "$obj->foo()"
    }
  }

  "a call from a function from an external namespace should be static and have a fully qualified name" in {
    val cpg = code("""
        |<?php
        |use function Foo\Bar\baz;
        |
        |baz();
        |""".stripMargin)

    val baz = cpg.call("baz").head
    baz.name shouldBe "baz"
    baz.methodFullName shouldBe "Foo\\Bar\\baz"
    baz.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
  }

  "calls from an external (lowercased) type" should {
    val cpg = code("""
        |<?php
        |use Foo\Bar\baz;
        |
        |baz::test1();
        |(new baz)->test2();
        |""".stripMargin)

    "be fully qualified in the case of static calls" in {
      val test1 = cpg.call("test1").head
      test1.name shouldBe "test1"
      test1.methodFullName shouldBe "Foo\\Bar\\baz<metaclass>.test1"
      test1.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "be unknown in the case of dynamic calls" in {
      val test2 = cpg.call("test2").head
      test2.name shouldBe "test2"
      test2.methodFullName shouldBe s"${Defines.UnresolvedNamespace}.test2"
      test2.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }

  "a chained call with a lambda argument should generate precisely one lambda reference" in {
    val cpg = code("""
        |<?php
        |class Foo {
        | public function bar() {
        |        $batches = 0;
        |        BatchBuilder::factory()
        |            ->transferWith(function () {
        |                $batches++;
        |            })
        |            ->build();
        |   }
        |}
        |""".stripMargin)

    cpg.expression.whereNot(_.astParent).size shouldBe 0
    cpg.call.argument.isMethodRef.size shouldBe 1
    val lambdaRef = cpg.call.argument.isMethodRef.head
    lambdaRef.methodFullName shouldBe "Foo.bar.<lambda>0"
  }

  "a call to a constructor should be static and have a correctly inferred method full name" in {
    val cpg = code("""
        |<?php
        |use NNN\Foo;
        |
        |$foo = new NNN\Foo();
        |""".stripMargin)

    val construct = cpg.call.nameExact(Domain.ConstructorMethodName).head

    construct.methodFullName shouldBe s"NNN\\Foo.${Domain.ConstructorMethodName}"
    construct.code shouldBe s"new NNN\\Foo()"
    construct.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    construct.typeFullName shouldBe Defines.Any
    construct.dynamicTypeHintFullName shouldBe Seq.empty
  }

}
