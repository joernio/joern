package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ClosureBinding, Identifier, Local, MethodRef, Return}
import io.shiftleft.semanticcpg.language._

class ClosureTests extends PhpCode2CpgFixture {

  "long-form closures without uses " ignore {
    val cpg = code("""<?php
     |$x = function($value) {
     |  echo $value;
     |};
     |""".stripMargin)

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*closure.*").l) { case List(closureMethod) =>
        closureMethod
      }

      closureMethod.name shouldBe "__closure0"
      closureMethod.fullName shouldBe s"__closure0:${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe "function __closure0($value)"
      closureMethod.parameter.size shouldBe 1

      inside(closureMethod.parameter.l) { case List(valueParam) =>
        valueParam.name shouldBe "value"
      }

      inside(closureMethod.body.astChildren.l) { case List(echoCall: Call) =>
        echoCall.code shouldBe "echo $value"
      }
    }

    "have a correct MethodRef added to the AST where the closure is defined" in {
      inside(cpg.assignment.argument.l) { case List(_: Identifier, methodRef: MethodRef) =>
        methodRef.methodFullName shouldBe s"__closure0:${Defines.UnresolvedSignature}(1)"
        methodRef.code shouldBe s"__closure0:${Defines.UnresolvedSignature}(1)"
        methodRef.lineNumber shouldBe Some(2)
      }
    }
  }

  "long-form closures with uses " should {
    val cpg = code("""<?php
                    |$use1 = "FOO";
                    |$x = function($value) use($use1, &$use2) {
                    |  echo $value;
                    |};
                    |""".stripMargin)

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*closure.*").l) { case List(closureMethod) =>
        closureMethod
      }

      closureMethod.name shouldBe "__closure0"
      closureMethod.fullName shouldBe s"__closure0"
      closureMethod.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe "function __closure0($value) use($use1, &$use2)"
      closureMethod.parameter.size shouldBe 1

      inside(closureMethod.parameter.l) { case List(valueParam) =>
        valueParam.name shouldBe "value"
      }

      inside(closureMethod.body.astChildren.l) { case List(use1: Local, use2: Local, echoCall: Call) =>
        use1.name shouldBe "use1"
        use1.code shouldBe "$use1"
        use1.closureBindingId.isEmpty shouldBe false
        inside(cpg.all.collectAll[ClosureBinding].filter(_.closureBindingId == use1.closureBindingId).l) {
          case List(closureBinding) =>
            closureBinding.closureOriginalName shouldBe Some("use1")
        }

        use2.name shouldBe "use2"
        use2.code shouldBe "&$use2"

        echoCall.code shouldBe "echo $value"
      }
    }

    "have a correct MethodRef added to the AST where the closure is defined" in {
      inside(cpg.assignment.code(".*closure.*").argument.l) { case List(_: Identifier, methodRef: MethodRef) =>
        methodRef.methodFullName shouldBe s"__closure0"
        methodRef.code shouldBe s"__closure0"
        methodRef.lineNumber shouldBe Some(3)
      }
    }
  }

  "arrow functions should be represented as closures with return statements" should {
    val cpg = code("""<?php
     |$x = fn ($value) => $value + 1;
     |""".stripMargin)

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*closure.*").l) { case List(closureMethod) =>
        closureMethod
      }

      closureMethod.name shouldBe "__closure0"
      closureMethod.fullName shouldBe s"__closure0"
      closureMethod.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe "function __closure0($value)"
      closureMethod.parameter.size shouldBe 1

      inside(closureMethod.parameter.l) { case List(valueParam) =>
        valueParam.name shouldBe "value"
      }

      inside(closureMethod.body.astChildren.l) { case List(methodReturn: Return) =>
        methodReturn.code shouldBe "return $value + 1"
      }
    }

    "have a correct MethodRef added to the AST where the closure is defined" in {
      inside(cpg.assignment.argument.l) { case List(_: Identifier, methodRef: MethodRef) =>
        methodRef.methodFullName shouldBe s"__closure0"
        methodRef.code shouldBe s"__closure0"
        methodRef.lineNumber shouldBe Some(2)
      }
    }
  }
}
