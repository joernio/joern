package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
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
    inside(contentsCall.astChildren.l) { case (receiver: Call) :: Nil =>
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "$a->contents"
      receiver.argumentIndex shouldBe 0
    }

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
      fooCall.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      fooCall.receiver.isEmpty shouldBe false
      fooCall.receiver.foreach { recvNode =>
        val recv = recvNode.asInstanceOf[Identifier]
        recv.code shouldBe "foo"
        recv.name shouldBe "foo"
      }

      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
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
        fooCall.methodFullName shouldBe s"Foo::foo"
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
      barCall.methodFullName shouldBe s"ClassA::bar"
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
      fooCall.methodFullName shouldBe """<unresolvedNamespace>\foo"""
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
      fooCall.methodFullName shouldBe """<unresolvedNamespace>\$foo"""
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      fooCall.lineNumber shouldBe Some(2)
      fooCall.code shouldBe "$$f->$foo($x)"

      inside(fooCall.astChildren.l) { case (fBase: Call) :: (fRecv: Identifier) :: (xArg: Identifier) :: Nil =>
        fBase.name shouldBe Operators.fieldAccess
        fBase.code shouldBe "$$f->$foo"
        fBase.lineNumber shouldBe Some(2)
        fBase.argumentIndex shouldBe -1

        inside(fBase.argument.l) { case List(fVar: Identifier, fooVar: FieldIdentifier) =>
          fVar.name shouldBe "f"
          fVar.code shouldBe "$$f"

          fooVar.canonicalName shouldBe "foo"
          fooVar.code shouldBe "$foo"
        }

        fRecv.name shouldBe "f"
        fRecv.argumentIndex shouldBe 0

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
    foo.inAssignment.code.head shouldBe "tmp0 = $obj->foo()"
    foo.code shouldBe "$obj->foo()"
    inside(cpg.call("foo").astChildren.l) { case (fa: Call) :: (recv: Identifier) :: Nil =>
      fa.name shouldBe Operators.fieldAccess
      fa.code shouldBe "$obj->foo"

      recv.name shouldBe "obj"
      recv.code shouldBe "$obj"
    }

    val bar = cpg.call("bar").head
    bar.code shouldBe "tmp0->bar()"
    inside(cpg.call("bar").astChildren.l) { case (fa: Call) :: (recv: Identifier) :: Nil =>
      fa.name shouldBe Operators.fieldAccess
      fa.code shouldBe "tmp0->bar"

      recv.name shouldBe "tmp0"
      recv.code shouldBe "tmp0"
    }
  }
}
