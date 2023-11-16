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

      closureMethod.name shouldBe "<lambda>0"
      closureMethod.fullName shouldBe s"<lambda>0:${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe "function <lambda>0($value)"
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
        methodRef.methodFullName shouldBe s"<lambda>0:${Defines.UnresolvedSignature}(1)"
        methodRef.code shouldBe s"<lambda>0:${Defines.UnresolvedSignature}(1)"
        methodRef.lineNumber shouldBe Some(2)
      }
    }
  }

  "long-form closures with uses " should {
    val cpg = code(
      """<?php
                    |$use1 = "FOO";
                    |$x = function($value) use($use1, &$use2) {
                    |  echo $value;
                    |};
                    |""".stripMargin,
      fileName = "foo.php"
    )

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*<lambda>.*").l) { case List(closureMethod) =>
        closureMethod
      }

      val expectedName = s"foo.php:<global>-><lambda>0"
      closureMethod.name shouldBe expectedName
      closureMethod.fullName shouldBe expectedName
      closureMethod.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe s"function $expectedName($$value) use($$use1, &$$use2)"
      closureMethod.parameter.size shouldBe 1

      inside(closureMethod.parameter.l) { case List(valueParam) =>
        valueParam.name shouldBe "value"
      }

      inside(closureMethod.body.astChildren.l) { case List(use1: Local, use2: Local, echoCall: Call) =>
        use1.name shouldBe "use1"
        use1.code shouldBe "$use1"
        use1.closureBindingId shouldBe Some(s"foo.php:$expectedName:use1")
        inside(cpg.all.collectAll[ClosureBinding].filter(_.closureBindingId == use1.closureBindingId).l) {
          case List(closureBinding) =>
            closureBinding.closureOriginalName shouldBe Some("use1")
        }

        use2.name shouldBe "use2"
        use2.code shouldBe "&$use2"
        use2.closureBindingId shouldBe Some(s"foo.php:$expectedName:use2")

        echoCall.code shouldBe "echo $value"
      }
    }

    "have a ref edge from the closure binding for a use to the captured node" in {
      inside(cpg.all.collectAll[ClosureBinding].filter(_.closureOriginalName.contains("use1")).l) {
        case List(closureBinding) =>
          val capturedNode = cpg.method.nameExact("<global>").local.name("use1").head
          closureBinding.refOut.toList shouldBe List(capturedNode)
      }
    }

    "have a correct MethodRef added to the AST where the closure is defined" in {
      inside(cpg.assignment.code(".*<lambda>.*").argument.l) { case List(_: Identifier, methodRef: MethodRef) =>
        val expectedName = s"foo.php:<global>-><lambda>0"
        methodRef.methodFullName shouldBe expectedName
        methodRef.code shouldBe expectedName
        methodRef.lineNumber shouldBe Some(3)
      }
    }
  }

  "arrow functions should be represented as closures with return statements" should {
    val cpg = code(
      """<?php
     |$x = fn ($value) => $value + 1;
     |""".stripMargin,
      fileName = "foo.php"
    )

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*<lambda>.*").l) { case List(closureMethod) =>
        closureMethod
      }

      val expectedName = "foo.php:<global>-><lambda>0"
      closureMethod.name shouldBe expectedName
      closureMethod.fullName shouldBe expectedName
      closureMethod.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      closureMethod.code shouldBe s"function $expectedName($$value)"
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
        val expectedName = "foo.php:<global>-><lambda>0"
        methodRef.methodFullName shouldBe expectedName
        methodRef.code shouldBe expectedName
        methodRef.lineNumber shouldBe Some(2)
      }
    }
  }

  "multiple closures in the same file should have the correct names" in {
    val cpg = code("""<?php
     |function foo() {
     |  $x = fn ($value) => $value + 1;
     |  $y = function ($value) {
     |    return $value + 2;
     |  }
     |}
     |
     |class Bar {
     |  function bar() {
     |    $x = fn ($value) => $value + 1;
     |    $y = function ($value) {
     |      return $value + 2;
     |    }
     |  }
     |}
     |""".stripMargin)

    inside(cpg.method.name(".*<lambda>.*").fullName.sorted.l) { case List(bar0, bar1, foo0, foo1) =>
      bar0 shouldBe "Bar->bar-><lambda>2"
      bar1 shouldBe "Bar->bar-><lambda>3"
      foo0 shouldBe "foo-><lambda>0"
      foo1 shouldBe "foo-><lambda>1"
    }
  }
}
