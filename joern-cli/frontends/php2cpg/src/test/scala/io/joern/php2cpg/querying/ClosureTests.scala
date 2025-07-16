package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

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

  "long-form closures using captured parameters" should {
    val cpg = code("""<?php
        |function foo($captured)
        |  function () use ($captured) {
        |    return $captured;
        |  }
        |}
        |""".stripMargin)

    "not create orphan identifiers" in {
      cpg.identifier.filter(node => Try(node.astParent.toList).isFailure).toSet shouldBe Set.empty
    }
  }

  "long-form closures with uses " should {
    val cpg = code(
      """<?php
        |$use1 = "FOO";
        |$x = function($value) use($use1, &$use2) {
        |  sink($value, $use1);
        |};
        |""".stripMargin,
      fileName = "foo.php"
    )

    "not create an orphan identifier for the use" in {
      cpg.identifier.filter(node => Try(node.astParent.toList).isFailure).toSet shouldBe Set.empty
    }

    "have the correct method AST" in {
      val closureMethod = inside(cpg.method.name(".*<lambda>.*").l) { case List(closureMethod) =>
        closureMethod
      }

      val expectedName = s"foo.php:<global>.<lambda>0"
      closureMethod.name shouldBe expectedName
      closureMethod.fullName shouldBe expectedName
      closureMethod.signature shouldBe ""
      closureMethod.code shouldBe s"function $expectedName($$value) use($$use1, &$$use2)"
      closureMethod.parameter.size shouldBe 1

      inside(closureMethod.parameter.l) { case List(valueParam) =>
        valueParam.name shouldBe "value"
      }

      inside(closureMethod.body.astChildren.l) { case List(use1: Local, use2: Local, echoCall: Call) =>
        use1.name shouldBe "use1"
        use1.code shouldBe "$use1"
        use1.closureBindingId shouldBe Some(s"$expectedName:use1")
        inside(cpg.all.collectAll[ClosureBinding].filter(_.closureBindingId == use1.closureBindingId).l) {
          case List(closureBinding) => closureBinding._localViaRefOut.name.l shouldBe List("use1")
        }

        use2.name shouldBe "use2"
        use2.code shouldBe "&$use2"
        use2.closureBindingId shouldBe Some(s"$expectedName:use2")

        echoCall.code shouldBe "sink($value,$use1)"
      }
    }

    "have a ref edge from the closure binding for a use to the captured node" in {
      inside(cpg.all.collectAll[ClosureBinding].filter(_.closureBindingId.exists(_.endsWith("use1"))).l) {
        case List(closureBinding) =>
          val capturedNode = cpg.method.nameExact("<global>").local.name("use1").head
          closureBinding.refOut.toList shouldBe List(capturedNode)
      }
    }

    "have a correct MethodRef added to the AST where the closure is defined" in {
      inside(cpg.assignment.code(".*<lambda>.*").argument.l) { case List(_: Identifier, methodRef: MethodRef) =>
        val expectedName = s"foo.php:<global>.<lambda>0"
        methodRef.methodFullName shouldBe expectedName
        methodRef.code shouldBe expectedName
        methodRef.lineNumber shouldBe Some(3)
      }
    }
  }

  "arrow functions with captures" should {
    val cpg = code(
      """<?php
        |$captured = 5
        |$x = fn ($value) => $value + $captured;
        |""".stripMargin,
      fileName = "foo.php"
    )

    "not leave orphan identifiers" in {
      cpg.identifier.filter(node => Try(node.astParent.toList).isFailure).toList shouldBe Nil
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

      val expectedName = "foo.php:<global>.<lambda>0"
      closureMethod.name shouldBe expectedName
      closureMethod.fullName shouldBe expectedName
      closureMethod.signature shouldBe ""
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
        val expectedName = "foo.php:<global>.<lambda>0"
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
      bar0 shouldBe "Bar.bar.<lambda>2"
      bar1 shouldBe "Bar.bar.<lambda>3"
      foo0 shouldBe "foo.<lambda>0"
      foo1 shouldBe "foo.<lambda>1"
    }
  }

  "simple global creates closure binding with correct ref edge" in {
    val cpg = code("""<?php
        |$a = 10
        |function foo() {
        |  global $a;
        |}
        |""".stripMargin)

    inside(cpg.all.collectAll[ClosureBinding].filter(_.closureBindingId.exists(_.endsWith("a"))).l) {
      case List(closureBinding) =>
        val capturedNode = cpg.method.nameExact("<global>").local.name("a").head
        closureBinding.refOut.toList shouldBe List(capturedNode)
    }
  }

  "global in nested functions" should {
    val cpg = code("""<?php
        |$a = 10;
        |$c = 11;
        |$e = 13;
        |function foo() {
        |  function bar() {
        |    global $a;
        |    global $c;
        |    global $e;
        |    $a = 1;
        |    $b = $a;
        |    $c = 1;
        |    $d = $c;
        |    $e = 1;
        |    $f = $e;
        |  }
        |}
        |""".stripMargin)

    "local variable exists in bar" in {
      val localNodeA = cpg.method.name("bar").local.name("a").head
      localNodeA.closureBindingId shouldBe Some("Test0.php:foo.bar:a")

      val localNodeC = cpg.method.name("bar").local.name("c").head
      localNodeC.closureBindingId shouldBe Some("Test0.php:foo.bar:c")

      val localNodeE = cpg.method.name("bar").local.name("e").head
      localNodeE.closureBindingId shouldBe Some("Test0.php:foo.bar:e")
    }

    "identifier association to local in bar" in {
      val localNodeA = cpg.method.name("bar").local.name("a").head
      localNodeA.referencingIdentifiers.lineNumber(10).code.head shouldBe "$a"
      localNodeA.referencingIdentifiers.lineNumber(11).code.head shouldBe "$a"

      val localNodeC = cpg.method.name("bar").local.name("c").head
      localNodeC.referencingIdentifiers.lineNumber(12).code.head shouldBe "$c"
      localNodeC.referencingIdentifiers.lineNumber(13).code.head shouldBe "$c"

      val localNodeE = cpg.method.name("bar").local.name("e").head
      localNodeE.referencingIdentifiers.lineNumber(14).code.head shouldBe "$e"
      localNodeE.referencingIdentifiers.lineNumber(15).code.head shouldBe "$e"
    }

    "method ref of closure binding of bar in foo" in {
      val methodRefNode     = cpg.methodRefWithName("bar").head
      val closureBindingIds = methodRefNode._closureBindingViaCaptureOut.l.map(_.closureBindingId)

      closureBindingIds shouldBe List(
        Some("Test0.php:foo.bar:a"),
        Some("Test0.php:foo.bar:c"),
        Some("Test0.php:foo.bar:e")
      )
    }

    "local variable exists in foo" in {
      val localNodeA = cpg.method.name("foo").local.name("a").head
      localNodeA.closureBindingId shouldBe Some("Test0.php:foo:a")

      val localNodeC = cpg.method.name("foo").local.name("c").head
      localNodeC.closureBindingId shouldBe Some("Test0.php:foo:c")

      val localNodeD = cpg.method.name("foo").local.name("e").head
      localNodeD.closureBindingId shouldBe Some("Test0.php:foo:e")
    }

    "method reference closure binding of foo in global" in {
      val methodRefNode     = cpg.methodRefWithName("foo").head
      val closureBindingIds = methodRefNode._closureBindingViaCaptureOut.l.map(_.closureBindingId)

      closureBindingIds shouldBe List(Some("Test0.php:foo:a"), Some("Test0.php:foo:c"), Some("Test0.php:foo:e"))
    }

    "global variable" in {
      val localNode = cpg.method.name("<global>").local.name("a").head
      localNode.closureBindingId shouldBe None

      val localNodeC = cpg.method.name("<global>").local.name("c").head
      localNodeC.closureBindingId shouldBe None

      val localNodeE = cpg.method.name("<global>").local.name("e").head
      localNodeE.closureBindingId shouldBe None
    }

    "closure binding reference to global" in {
      val localNode = cpg.method.name("<global>").local.name("a").head
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("Test0.php:foo:a")

      val localNodeC = cpg.method.name("<global>").local.name("c").head
      localNodeC._closureBindingViaRefIn.next().closureBindingId shouldBe Some("Test0.php:foo:c")

      val localNodeE = cpg.method.name("<global>").local.name("e").head
      localNodeE._closureBindingViaRefIn.next().closureBindingId shouldBe Some("Test0.php:foo:e")
    }

    "should only have two methodRefs" in {
      inside(cpg.all.collectAll[MethodRef].l) {
        case fooMethodRef :: fooBarMethodRef :: Nil =>
          fooMethodRef.methodFullName shouldBe "foo"
          fooMethodRef.astParent.astParent.asInstanceOf[Method].fullName shouldBe "Test0.php:<global>"

          fooBarMethodRef.methodFullName shouldBe "foo.bar"
          fooBarMethodRef.astParent.astParent.asInstanceOf[Method].fullName shouldBe "foo"
        case xs => fail(s"Expected two methodRefs, got ${xs.methodFullName.mkString("[", ",", "]")}")
      }
    }
  }

  "Global variable in method constructor" should {
    val cpg = code("""<?php
        |$a = 10;
        |class Foo {
        |  function __construct() {
        |     global $a;
        |  }
        |}
        |""".stripMargin)

    "local variable exists in construct" in {
      val localNodeA = cpg.method.name("__construct").local.name("a").head
      localNodeA.closureBindingId shouldBe Some("Test0.php:Foo.__construct:a")
    }

    "method ref of closure binding of construct" in {
      val methodRefNode     = cpg.methodRefWithName("__construct").head
      val closureBindingIds = methodRefNode._closureBindingViaCaptureOut.l.map(_.closureBindingId)

      closureBindingIds shouldBe List(Some("Test0.php:Foo.__construct:a"))
    }

    "global variable" in {
      val localNode = cpg.method.name("<global>").local.name("a").head
      localNode.closureBindingId shouldBe None
    }

    "closure binding reference to global" in {
      val localNode = cpg.method.name("<global>").local.name("a").head
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("Test0.php:Foo.__construct:a")
    }

    "have a methodRef for construct" in {
      inside(cpg.all.collectAll[MethodRef].l) {
        case constructorRef :: Nil =>
          constructorRef.methodFullName shouldBe "Foo.__construct"
          constructorRef.astParent.astParent.asInstanceOf[Method].fullName shouldBe "Test0.php:<global>"
        case xs => fail(s"Expected one methodRef for constructor, got ${xs.methodFullName.mkString("[", ",", "]")}")
      }
    }
  }

  "conditional method definitions with global statements" should {
    val cpg = code("""<?php
                     |$a = 10;
                     |$b = 20;
                     |if (true) {
                     |  function foo() {
                     |    global $a;
                     |    global $b;
                     |  }
                     |} else {
                     |  function foo() {
                     |    global $a;
                     |    global $b;
                     |  }
                     |}
                     |""".stripMargin)

    "have no orphaned methodRef nodes" in {
      cpg.methodRefWithName("foo.*").filter(node => Try(node.astParent.toList).isFailure).toList shouldBe Nil
    }

    "local variable exists in foo" in {
      val localNodeA = cpg.method.fullName("foo").local.name("a").head
      localNodeA.closureBindingId shouldBe Some("Test0.php:foo:a")

      val localNodeB = cpg.method.fullName("foo").local.name("b").head
      localNodeB.closureBindingId shouldBe Some("Test0.php:foo:b")

      val localNodeADup = cpg.method.fullName("foo<duplicate>0").local.name("a").head
      localNodeADup.closureBindingId shouldBe Some("Test0.php:foo<duplicate>0:a")

      val localNodeBDup = cpg.method.fullName("foo<duplicate>0").local.name("b").head
      localNodeBDup.closureBindingId shouldBe Some("Test0.php:foo<duplicate>0:b")
    }

    "method ref of closure binding of construct" in {
      val methodRefNode     = cpg.methodRef.methodFullName("foo").head
      val closureBindingIds = methodRefNode._closureBindingViaCaptureOut.l.map(_.closureBindingId)

      val methodRefNodeDup     = cpg.methodRef.methodFullName("foo<duplicate>0").head
      val closureBindingIdsDup = methodRefNodeDup._closureBindingViaCaptureOut.l.map(_.closureBindingId)

      closureBindingIds shouldBe List(Some("Test0.php:foo:a"), Some("Test0.php:foo:b"))

      closureBindingIdsDup shouldBe List(Some("Test0.php:foo<duplicate>0:a"), Some("Test0.php:foo<duplicate>0:b"))
    }

    "global variable" in {
      val localNode  = cpg.method.name("<global>").local.name("a").head
      val localNodeB = cpg.method.name("<global>").local.name("b").head
      localNode.closureBindingId shouldBe None
      localNodeB.closureBindingId shouldBe None
    }

    "closure binding reference to global" in {
      val localNode  = cpg.method.name("<global>").local.name("a").head
      val localNodeB = cpg.method.name("<global>").local.name("b").head

      localNode._closureBindingViaRefIn.map(_.closureBindingId).l shouldBe List(
        Some("Test0.php:foo:a"),
        Some("Test0.php:foo<duplicate>0:a")
      )
      localNodeB._closureBindingViaRefIn.map(_.closureBindingId).l shouldBe List(
        Some("Test0.php:foo:b"),
        Some("Test0.php:foo<duplicate>0:b")
      )
    }

    "create two method refs" in {
      inside(cpg.methodRefWithName("foo.*").l) {
        case methodRef :: duplicateMethodRef :: Nil =>
          methodRef.methodFullName shouldBe "foo"
          duplicateMethodRef.methodFullName shouldBe "foo<duplicate>0"
        case xs => fail(s"Expected two method refs, got ${xs.methodFullName.mkString("[", ",", "]")}")
      }
    }
  }
}
