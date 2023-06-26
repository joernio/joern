package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.Block

class CallTests extends PhpCode2CpgFixture {
  "halt_compiler calls should be created correctly" in {
    val cpg = code("<?php\n__halt_compiler();")

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
    val cpg = code("<?php\nfoo($x);")

    inside(cpg.call.l) { case List(fooCall) =>
      fooCall.name shouldBe "foo"
      fooCall.methodFullName shouldBe s"foo"
      fooCall.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
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
    val cpg = code("<?php\nFoo::foo($x);")

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

  "method calls with simple names should be correct" in {
    val cpg = code("<?php\n$f->foo($x);")

    inside(cpg.call.l) { case List(fooCall) =>
      fooCall.name shouldBe "foo"
      fooCall.methodFullName shouldBe """<unresolvedNamespace>\$f->foo"""
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
    val cpg = code("<?php\n$$f->{$foo}($x)")

    inside(cpg.call.filter(_.name != Operators.fieldAccess).l) { case List(fooCall) =>
      fooCall.name shouldBe "$foo"
      fooCall.methodFullName shouldBe s"<unresolvedNamespace>\\$$$$f->$$foo"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      fooCall.lineNumber shouldBe Some(2)
      fooCall.code shouldBe "$$f->$foo($x)"

      inside(fooCall.argument.l) { case List(fRecv: Call, xArg: Identifier) =>
        fRecv.name shouldBe Operators.fieldAccess
        fRecv.code shouldBe "$$f->$foo"
        fRecv.lineNumber shouldBe Some(2)

        inside(fRecv.argument.l) { case List(fVar: Identifier, fooVar: Identifier) =>
          fVar.name shouldBe "f"
          fVar.code shouldBe "$$f"

          fooVar.name shouldBe "foo"
          fooVar.code shouldBe "$foo"
        }

        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }
}
