package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, MethodRef}
import io.shiftleft.semanticcpg.language._

class ClosureTests extends PhpCode2CpgFixture {

  "long-form closures without uses " should {
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
                    |$captured = "I'm CAPTURED"
                    |$x = function($value) {
                    |  echo $value;
                    |};
                    |""".stripMargin)

  }
}
