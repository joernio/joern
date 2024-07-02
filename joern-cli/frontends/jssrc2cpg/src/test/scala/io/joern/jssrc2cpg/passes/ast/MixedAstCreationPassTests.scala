package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language.*

class MixedAstCreationPassTests extends AstJsSrc2CpgSuite {

  "AST method full names" should {
    "anonymous arrow function full name 1" in {
      val cpg = code("var func = (x) => x;")
      cpg.method.fullName.toSetMutable should contain("Test0.js::program:<lambda>0")
    }
    "anonymous arrow function full name 2" in {
      val cpg = code("this.func = (x) => x;")
      cpg.method.fullName.toSetMutable should contain("Test0.js::program:<lambda>0")
    }
    "anonymous function expression full name 1" in {
      val cpg = code("var func = function (x) {x};")
      cpg.method.fullName.toSetMutable should contain("Test0.js::program:<lambda>0")
    }
    "anonymous function expression full name 2" in {
      val cpg = code("this.func = function (x) {x};")
      cpg.method.fullName.toSetMutable should contain("Test0.js::program:<lambda>0")
    }
    "anonymous constructor full name 1" in {
      val cpg = code("class X { constructor(){} }")
      cpg.method.fullName.toSetMutable should contain(
        s"Test0.js::program:X:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      )
    }
    "anonymous constructor of anonymous class full name" in {
      val cpg = code("var x = class { constructor(y) {} };")
      cpg.method.fullName.toSetMutable should contain(
        s"Test0.js::program:<anon-class>0:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      )
    }
  }

  "AST variable scoping and linking" should {
    "have correct references for single local var" in {
      val cpg = code("""
         |var x;
         |x = 1;
        """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for single local let" in {
      val cpg = code("""
         |let x;
         |x = 1;
        """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for undeclared local" in {
      val cpg                = code("x = 1;")
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for undeclared local with 2 refs" in {
      val cpg = code("""
         |x = 1;
         |x = 2;
       """.stripMargin)
      val List(method)        = cpg.method.nameExact(":program").l
      val List(methodBlock)   = method.astChildren.isBlock.l
      val List(localX)        = methodBlock.astChildren.isLocal.l
      val List(assignment1)   = methodBlock.astChildren.isCall.order(1).l
      val List(identifierX1)  = assignment1.astChildren.isIdentifier.l
      val List(localXViaRef1) = identifierX1.refOut.l
      localXViaRef1 shouldBe localX

      val List(assignment2)   = methodBlock.astChildren.isCall.order(2).l
      val List(identifierX2)  = assignment2.astChildren.isIdentifier.l
      val List(localXViaRef2) = identifierX2.refOut.l
      localXViaRef2 shouldBe localX
    }

    "have correct references for undeclared local in block" in {
      val cpg                = code("{ x = 1; }")
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(nestedBlock)  = methodBlock.astChildren.isBlock.l
      val List(assignment)   = nestedBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for single var in block" in {
      val cpg = code("""
        |{ var x; }
        |x = 1;
       """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(nestedBlock)  = methodBlock.astChildren.isBlock.l
      val List(localX)       = nestedBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for single post declared var" in {
      val cpg = code("""
         |x = 1;
         |var x;
        """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for single post declared var in block" in {
      val cpg = code("""
          |x = 1;
          |{ var x; }
        """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(nestedBlock)  = methodBlock.astChildren.isBlock.l
      val List(localX)       = nestedBlock.astChildren.isLocal.l
      val List(assignment)   = methodBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for single nested access to let" in {
      val cpg = code("""
          |let x;
          |{ x = 1; }
        """.stripMargin)
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodBlock)  = method.astChildren.isBlock.l
      val List(localX)       = methodBlock.astChildren.isLocal.l
      val List(nestedBlock)  = methodBlock.astChildren.isBlock.l
      val List(assignment)   = nestedBlock.astChildren.isCall.l
      val List(identifierX)  = assignment.astChildren.isIdentifier.l
      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX
    }

    "have correct references for shadowing let" in {
      val cpg = code("""
          |let x;
          |{
          |  let x;
          |  x = 1;
          |}
          |x = 1;
        """.stripMargin)
      val List(method)            = cpg.method.nameExact(":program").l
      val List(methodBlock)       = method.astChildren.isBlock.l
      val List(outerLocalX)       = methodBlock.astChildren.isLocal.l
      val List(nestedBlock)       = methodBlock.astChildren.isBlock.l
      val List(innerLocalX)       = nestedBlock.astChildren.isLocal.l
      val List(innerAssignment)   = nestedBlock.astChildren.isCall.l
      val List(innerIdentifierX)  = innerAssignment.astChildren.isIdentifier.l
      val List(innerLocalXViaRef) = innerIdentifierX.refOut.l
      innerLocalXViaRef shouldBe innerLocalX

      val List(outerAssignment)   = methodBlock.astChildren.isCall.l
      val List(outerIdentifierX)  = outerAssignment.astChildren.isIdentifier.l
      val List(outerLocalXViaRef) = outerIdentifierX.refOut.l
      outerLocalXViaRef shouldBe outerLocalX
    }

    "have correct closure binding (destructing parameter)" in {
      val cpg = code("""
        |const WindowOpen = ({ value }) => {
        |  return (
        |    <div>
        |      <Button variant="outlined" onClick={() => windowOpenButton(value)}>
        |        TRY ME!
        |      </Button>
        |    </div>
        |  );
        |};
        """.stripMargin)
      cpg.local.name("value").closureBindingId.l shouldBe List("Test0.js::program:<lambda>0:<lambda>1:value")
    }

    "have correct closure binding (argument to call)" in {
      val cpg = code(
        """
        |const opts: RequestInit = {
        |  method: "GET",
        |  headers,
        |};
        |
        |const fetchCookies = () => {
        |  fetch(`/api/echo/${inputString}`, opts)
        |};""".stripMargin,
        "code.ts"
      )
      cpg.local.name("opts").closureBindingId.l shouldBe List("code.ts::program:<lambda>0:opts")
    }

    "have correct closure binding (destructing assignment)" in {
      val cpg = code("""
        |const {closureA} = null;
        |const [closureB] = null;
        |let f = function() {
        |  console.log(closureA);
        |  console.log(closureB);
        |}
        """.stripMargin)
      cpg.local.name("closureA").closureBindingId.l shouldBe List("Test0.js::program:<lambda>0:closureA")
      cpg.local.name("closureB").closureBindingId.l shouldBe List("Test0.js::program:<lambda>0:closureB")
    }

    "have correct closure binding (single variable)" in {
      val cpg = code("""
         |function foo() {
         |  x = 1;
         |  function bar() {
         |    x = 2;
         |  }
         |}
        """.stripMargin)
      val List(fooMethod)      = cpg.method.nameExact("foo").l
      val List(fooBlock)       = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)      = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)         = fooBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBinding) = barRef.captureOut.l
      closureBinding.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBinding.closureOriginalName shouldBe Option("x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE

      closureBinding.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      val List(barLocals)      = barMethodBlock.astChildren.isLocal.l
      barLocals.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(identifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      identifierX.refOut.head shouldBe barLocals
    }

    "have correct closure binding (two variables)" in {
      val cpg = code("""
         |function foo() {
         |  x = 1;
         |  y = 1;
         |  function bar() {
         |    x = 2;
         |    y = 2;
         |  }
         |}
        """.stripMargin)
      val List(fooMethod) = cpg.method.nameExact("foo").l
      val List(fooBlock)  = fooMethod.astChildren.isBlock.l
      val List(fooLocalX) = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(fooLocalY) = fooBlock.astChildren.isLocal.nameExact("y").l
      val List(barRef)    = fooBlock.astChildren.isCall.astChildren.isMethodRef.l

      val List(closureBindForY, closureBindForX) = barRef.captureOut.l

      closureBindForX.closureOriginalName shouldBe Option("x")
      closureBindForX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBindForX.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindForX.refOut.head shouldBe fooLocalX

      closureBindForY.closureOriginalName shouldBe Option("y")
      closureBindForY.closureBindingId shouldBe Option("Test0.js::program:foo:bar:y")
      closureBindForY.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindForY.refOut.head shouldBe fooLocalY

      val List(barMethod)                    = cpg.method.nameExact("bar").l
      val List(barMethodBlock)               = barMethod.astChildren.isBlock.l
      val List(barLocalsForY, barLocalsForX) = barMethodBlock.astChildren.isLocal.l

      barLocalsForX.name shouldBe "x"
      barLocalsForX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(identifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      identifierX.refOut.head shouldBe barLocalsForX

      barLocalsForY.name shouldBe "y"
      barLocalsForY.closureBindingId shouldBe Option("Test0.js::program:foo:bar:y")

      val List(identifierY) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("y").l
      identifierY.refOut.head shouldBe barLocalsForY
    }

    "have correct closure binding for capturing over 2 levels" in {
      val cpg = code("""
         |function foo() {
         |  x = 1;
         |  function bar() {
         |    x = 2;
         |    function baz() {
         |      x = 3;
         |    }
         |  }
         |}
        """.stripMargin)
      val List(fooMethod) = cpg.method.nameExact("foo").l
      val List(fooBlock)  = fooMethod.astChildren.isBlock.l
      val List(fooLocalX) = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)    = fooBlock.astChildren.isCall.astChildren.isMethodRef.l

      val List(closureBindingXInFoo) = barRef.captureOut.l
      closureBindingXInFoo.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBindingXInFoo.closureOriginalName shouldBe Option("x")
      closureBindingXInFoo.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInFoo.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l

      val List(barLocalX) = barMethodBlock.astChildren.isLocal.nameExact("x").l
      barLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(barIdentifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      barIdentifierX.refOut.head shouldBe barLocalX

      val List(bazRef)               = barMethodBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBindingXInBar) = bazRef.captureOut.l
      closureBindingXInBar.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")
      closureBindingXInBar.closureOriginalName shouldBe Option("x")
      closureBindingXInBar.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInBar.refOut.head shouldBe barLocalX

      val List(bazMethod)      = cpg.method.nameExact("baz").l
      val List(bazMethodBlock) = bazMethod.astChildren.isBlock.l
      val List(bazLocalX)      = bazMethodBlock.astChildren.isLocal.nameExact("x").l
      bazLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")

      val List(bazIdentifierX) = bazMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      bazIdentifierX.refOut.head shouldBe bazLocalX
    }

    "have correct closure binding for capturing over 2 levels with intermediate blocks" in {
      val cpg = code("""
          |function foo() {
          |  x = 1;
          |  function bar() {
          |    x = 2;
          |    {
          |      function baz() {
          |        {
          |          x = 3;
          |        }
          |      }
          |    }
          |  }
          |}
        """.stripMargin)
      val List(fooMethod)            = cpg.method.nameExact("foo").l
      val List(fooBlock)             = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)            = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)               = fooBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBindingXInFoo) = barRef.captureOut.l
      closureBindingXInFoo.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBindingXInFoo.closureOriginalName shouldBe Option("x")
      closureBindingXInFoo.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInFoo.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l

      val List(barLocalX) = barMethodBlock.astChildren.isLocal.nameExact("x").l
      barLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(barIdentifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      barIdentifierX.refOut.head shouldBe barLocalX

      val List(barMethodInnerBlock)  = barMethodBlock.astChildren.isBlock.l
      val List(bazRef)               = barMethodInnerBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBindingXInBar) = bazRef.captureOut.l
      closureBindingXInBar.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")
      closureBindingXInBar.closureOriginalName shouldBe Option("x")
      closureBindingXInBar.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInBar.refOut.head shouldBe barLocalX

      val List(bazMethod)      = cpg.method.nameExact("baz").l
      val List(bazMethodBlock) = bazMethod.astChildren.isBlock.l

      val List(bazLocalX) = bazMethodBlock.astChildren.isLocal.nameExact("x").l
      bazLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")

      val List(bazMethodInnerBlock) = bazMethodBlock.astChildren.isBlock.l
      val List(bazIdentifierX)      = bazMethodInnerBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      bazIdentifierX.refOut.head shouldBe bazLocalX
    }

    "have correct closure binding for capturing over 2 levels with no intermediate use" in {
      val cpg = code("""
          |function foo() {
          |  x = 1;
          |  function bar() {
          |    function baz() {
          |      x = 3;
          |    }
          |  }
          |}
        """.stripMargin)
      val List(fooMethod)            = cpg.method.nameExact("foo").l
      val List(fooBlock)             = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)            = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)               = fooBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBindingXInFoo) = barRef.captureOut.l
      closureBindingXInFoo.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBindingXInFoo.closureOriginalName shouldBe Option("x")
      closureBindingXInFoo.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInFoo.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l

      val List(barLocalX) = barMethodBlock.astChildren.isLocal.nameExact("x").l
      barLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(bazRef)               = barMethodBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBindingXInBar) = bazRef.captureOut.l
      closureBindingXInBar.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")
      closureBindingXInBar.closureOriginalName shouldBe Option("x")
      closureBindingXInBar.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXInBar.refOut.head shouldBe barLocalX

      val List(bazMethod)      = cpg.method.nameExact("baz").l
      val List(bazMethodBlock) = bazMethod.astChildren.isBlock.l

      val List(bazLocalX) = bazMethodBlock.astChildren.isLocal.nameExact("x").l
      bazLocalX.closureBindingId shouldBe Option("Test0.js::program:foo:bar:baz:x")

      val List(bazIdentifierX) = bazMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      bazIdentifierX.refOut.head shouldBe bazLocalX
    }

    "have correct closure binding for capturing the same variable into 2 different anonymous methods" in {
      val cpg = code("""
          |function foo() {
          |  var x = 1;
          |  var anon1 = y => 2*x;
          |  var anon2 = y => 2*x;
          |}
        """.stripMargin)
      val List(fooMethod) = cpg.method.nameExact("foo").l
      val List(fooBlock)  = fooMethod.astChildren.isBlock.l
      val List(fooLocalX) = fooBlock.astChildren.isLocal.nameExact("x").l

      val List(anon1Ref) =
        fooBlock.astChildren.isCall.astChildren.isMethodRef.methodFullNameExact("Test0.js::program:foo:<lambda>0").l

      val List(closureBindingXAnon1) = anon1Ref.captureOut.l
      closureBindingXAnon1.closureBindingId shouldBe Option("Test0.js::program:foo:<lambda>0:x")
      closureBindingXAnon1.closureOriginalName shouldBe Option("x")
      closureBindingXAnon1.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXAnon1.refOut.head shouldBe fooLocalX

      val List(anon2Ref) =
        fooBlock.astChildren.isCall.astChildren.isMethodRef.methodFullNameExact("Test0.js::program:foo:<lambda>1").l
      val List(closureBindingXAnon2) = anon2Ref.captureOut.l
      closureBindingXAnon2.closureBindingId shouldBe Option("Test0.js::program:foo:<lambda>1:x")
      closureBindingXAnon2.closureOriginalName shouldBe Option("x")
      closureBindingXAnon2.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBindingXAnon2.refOut.head shouldBe fooLocalX
    }

    "have correct closure bindings" in {
      val cpg = code("""
        |function foo() {
        |  x = 1;
        |  function bar() {
        |    x = 2;
        |  }
        |}
        |""".stripMargin)
      val List(fooMethod)      = cpg.method.nameExact("foo").l
      val List(fooBlock)       = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)      = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)         = fooBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBinding) = barRef.captureOut.l
      closureBinding.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")
      closureBinding.closureOriginalName shouldBe Option("x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      val List(barLocals)      = barMethodBlock.astChildren.isLocal.l
      barLocals.closureBindingId shouldBe Option("Test0.js::program:foo:bar:x")

      val List(identifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      identifierX.refOut.head shouldBe barLocals
    }

    "have correct method full names for scoped anonymous functions" in {
      val cpg = code("""
        |var anon1 = x => {
        |  var anon2 = y => {};
        |}
        |var anon3 = x => {
        |  var anon4 = y => {};
        |}""".stripMargin)
      cpg.method.lineNumber(2).head.fullName shouldBe "Test0.js::program:<lambda>0"
      cpg.method.lineNumber(3).head.fullName shouldBe "Test0.js::program:<lambda>0:<lambda>1"
      cpg.method.lineNumber(5).head.fullName shouldBe "Test0.js::program:<lambda>2"
      cpg.method.lineNumber(6).head.fullName shouldBe "Test0.js::program:<lambda>2:<lambda>3"
    }
  }

  "AST generation for mixed fragments" should {
    "simple js fragment with call" in {
      val cpg = code("""
         |function source(a) { return a; }
         |var l = source(3);
        """.stripMargin)
      val List(program)      = cpg.method.nameExact(":program").l
      val List(method)       = cpg.method.nameExact("source").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(methodBlock)  = method.astChildren.isBlock.l
      method.parameter.size shouldBe 2

      val List(localSource, localL) = programBlock.astChildren.isLocal.l
      localSource.name shouldBe "source"
      localSource.typeFullName shouldBe "Test0.js::program:source"
      localL.name shouldBe "l"

      val List(callToSource) = programBlock.astChildren.isCall.codeExact("var l = source(3)").l

      val List(identifierL) = callToSource.astChildren.isIdentifier.l
      identifierL.name shouldBe "l"

      val List(call) = callToSource.astChildren.isCall.l
      call.astChildren.isLiteral.codeExact("3").size shouldBe 1

      val List(returnFromMethod) = methodBlock.astChildren.isReturn.l
      returnFromMethod.astChildren.isIdentifier.nameExact("a").size shouldBe 1
    }

    "simple js fragment with array access" in {
      val cpg                = code("result = rows[0].solution;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(call)         = programBlock.astChildren.isCall.l
      val List(rowsCall)     = call.astChildren.isCall.l
      rowsCall.astChildren.isFieldIdentifier.canonicalNameExact("solution").size shouldBe 1

      val List(rowsCallLeft) = rowsCall.astChildren.isCall.l
      rowsCallLeft.astChildren.isLiteral.codeExact("0").size shouldBe 1
      rowsCallLeft.astChildren.isIdentifier.nameExact("rows").size shouldBe 1
      call.astChildren.isIdentifier.nameExact("result").size shouldBe 1
    }

  }

  "AST generation for destructing assignment" should {
    "have correct structure for object destruction assignment with declaration" in {
      val cpg                = code("var {a, b} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l

      val List(localA) = cpg.local.nameExact("a").l
      val List(localB) = cpg.local.nameExact("b").l
      localA.referencingIdentifiers.name.head shouldBe "a"
      localB.referencingIdentifiers.name.head shouldBe "b"

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.code shouldBe "var {a, b} = x"
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(localTmp) = destructionBlock.astChildren.isLocal.nameExact("_tmp_0").l
      localTmp.referencingIdentifiers.name.head shouldBe "_tmp_0"

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessA.name shouldBe Operators.fieldAccess
      fieldAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessA.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0.b").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0.b").l
      fieldAccessB.name shouldBe Operators.fieldAccess
      fieldAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessB.astChildren.isFieldIdentifier.canonicalNameExact("b").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with declaration and ternary init" in {
      val cpg                = code("const { a, b } = test() ? foo() : bar();")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = test() ? foo() : bar()").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessA.name shouldBe Operators.fieldAccess
      fieldAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessA.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0.b").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0.b").l
      fieldAccessB.name shouldBe Operators.fieldAccess
      fieldAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessB.astChildren.isFieldIdentifier.canonicalNameExact("b").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment without declaration" in {
      val cpg                = code("({a, b} = x);")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessA.name shouldBe Operators.fieldAccess
      fieldAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessA.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0.b").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0.b").l
      fieldAccessB.name shouldBe Operators.fieldAccess
      fieldAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessB.astChildren.isFieldIdentifier.canonicalNameExact("b").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with defaults" in {
      val cpg                = code("var {a = 1, b = 2} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      programBlock.astChildren.isLocal.nameExact("a").size shouldBe 1
      programBlock.astChildren.isLocal.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.assignment.codeExact("a = _tmp_0.a === void 0 ? 1 : _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(ifA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a === void 0 ? 1 : _tmp_0.a").l
      ifA.name shouldBe Operators.conditional

      val List(testA) = ifA.astChildren.isCall.codeExact("_tmp_0.a === void 0").l
      testA.name shouldBe Operators.equals

      val List(testAFieldAccess) = testA.astChildren.isCall.codeExact("_tmp_0.a").l
      testAFieldAccess.name shouldBe Operators.fieldAccess

      testA.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifA.astChildren.isLiteral.codeExact("1").size shouldBe 1

      val List(falseBranchA) = ifA.astChildren.isCall.codeExact("_tmp_0.a").l
      falseBranchA.name shouldBe Operators.fieldAccess

      val List(assignmentToB) = destructionBlock.assignment.codeExact("b = _tmp_0.b === void 0 ? 2 : _tmp_0.b").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(ifB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0.b === void 0 ? 2 : _tmp_0.b").l
      ifB.name shouldBe Operators.conditional

      val List(testB) = ifB.astChildren.isCall.codeExact("_tmp_0.b === void 0").l
      testB.name shouldBe Operators.equals

      val List(testBFieldAccess) = testB.astChildren.isCall.codeExact("_tmp_0.b").l
      testBFieldAccess.name shouldBe Operators.fieldAccess

      testB.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifB.astChildren.isLiteral.codeExact("2").size shouldBe 1

      val List(falseBranchB) = ifB.astChildren.isCall.codeExact("_tmp_0.b").l
      falseBranchB.name shouldBe Operators.fieldAccess

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with reassignment" in {
      val cpg                = code("var {a: n, b: m} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("n").size shouldBe 1
      cpg.local.nameExact("m").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToN) = destructionBlock.astChildren.isCall.codeExact("n = _tmp_0.a").l
      assignmentToN.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessN) = assignmentToN.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessN.name shouldBe Operators.fieldAccess
      fieldAccessN.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessN.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(assignmentToM) = destructionBlock.astChildren.isCall.codeExact("m = _tmp_0.b").l
      assignmentToM.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessM) = assignmentToM.astChildren.isCall.codeExact("_tmp_0.b").l
      fieldAccessM.name shouldBe Operators.fieldAccess
      fieldAccessM.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessM.astChildren.isFieldIdentifier.canonicalNameExact("b").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with reassignment and defaults" in {
      val cpg                = code("var {a: n = 1, b: m = 2} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      programBlock.astChildren.isLocal.nameExact("n").size shouldBe 1
      programBlock.astChildren.isLocal.nameExact("m").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToN) = destructionBlock.assignment.codeExact("n = _tmp_0.a === void 0 ? 1 : _tmp_0.a").l
      assignmentToN.astChildren.isIdentifier.size shouldBe 1

      val List(ifA) = assignmentToN.astChildren.isCall.codeExact("_tmp_0.a === void 0 ? 1 : _tmp_0.a").l
      ifA.name shouldBe Operators.conditional

      val List(testA) = ifA.astChildren.isCall.codeExact("_tmp_0.a === void 0").l
      testA.name shouldBe Operators.equals

      val List(testAFieldAccess) = testA.astChildren.isCall.codeExact("_tmp_0.a").l
      testAFieldAccess.name shouldBe Operators.fieldAccess

      testA.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifA.astChildren.isLiteral.codeExact("1").size shouldBe 1

      val List(falseBranchA) = ifA.astChildren.isCall.codeExact("_tmp_0.a").l
      falseBranchA.name shouldBe Operators.fieldAccess

      val List(assignmentToM) = destructionBlock.assignment.codeExact("m = _tmp_0.b === void 0 ? 2 : _tmp_0.b").l
      assignmentToN.astChildren.isIdentifier.size shouldBe 1

      val List(ifB) = assignmentToM.astChildren.isCall.codeExact("_tmp_0.b === void 0 ? 2 : _tmp_0.b").l
      ifB.name shouldBe Operators.conditional

      val List(testB) = ifB.astChildren.isCall.codeExact("_tmp_0.b === void 0").l
      testB.name shouldBe Operators.equals

      val List(testBFieldAccess) = testB.astChildren.isCall.codeExact("_tmp_0.b").l
      testBFieldAccess.name shouldBe Operators.fieldAccess

      testB.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifB.astChildren.isLiteral.codeExact("2").size shouldBe 1

      val List(falseBranchB) = ifB.astChildren.isCall.codeExact("_tmp_0.b").l
      falseBranchB.name shouldBe Operators.fieldAccess

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct ref edge (destructing parameter)" in {
      val cpg = code("""
        |const WindowOpen = ({ value }) => {
        |  return (
        |    <div>
        |      <Button variant="outlined" onClick={() => windowOpenButton(value)}>
        |        TRY ME!
        |      </Button>
        |    </div>
        |  );
        |};
        """.stripMargin)
      val List(param) = cpg.identifier.name("param1_0").refsTo.collectAll[MethodParameterIn].l
      param.name shouldBe "param1_0"
      param.code shouldBe "{ value }"
      param.method.fullName shouldBe "Test0.js::program:<lambda>0"
    }

    "have correct structure for object deconstruction in function parameter" in {
      val cpg             = code("function foo({ a }, b) {};")
      val List(program)   = cpg.method.nameExact(":program").l
      val List(fooMethod) = program.astChildren.isMethod.nameExact("foo").l
      val List(a)         = fooMethod.parameter.nameExact("param1_0").l
      a.code shouldBe "{ a }"
      a.index shouldBe 1
      val List(b) = fooMethod.parameter.nameExact("b").l
      b.code shouldBe "b"
      b.index shouldBe 2
    }

    "have correct structure for object destruction assignment in call argument" in {
      val cpg                = code("foo({a, b} = x);")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(fooCall)          = programBlock.astChildren.isCall.l
      val List(destructionBlock) = fooCall.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessA.name shouldBe Operators.fieldAccess
      fieldAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessA.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0.b").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0.b").l
      fieldAccessB.name shouldBe Operators.fieldAccess
      fieldAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessB.astChildren.isFieldIdentifier.canonicalNameExact("b").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with rest" in {
      val cpg                = code("var {a, ...rest} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("rest").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0.a").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0.a").l
      fieldAccessA.name shouldBe Operators.fieldAccess
      fieldAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessA.astChildren.isFieldIdentifier.canonicalNameExact("a").size shouldBe 1

      val List(restCall) = destructionBlock.astChildren.isCall.nameExact("<operator>.spread").l
      restCall.code shouldBe "...rest"
      val List(tmpArg, restArg) = restCall.argument.isIdentifier.l
      tmpArg.code shouldBe "_tmp_0"
      tmpArg.name shouldBe "_tmp_0"
      tmpArg.argumentIndex shouldBe 1
      restArg.code shouldBe "rest"
      restArg.argumentIndex shouldBe 2

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for object destruction assignment with computed property name" in {
      val cpg                = code("var {[propName]: n} = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("n").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToN) = destructionBlock.astChildren.isCall.codeExact("n = _tmp_0.propName").l
      assignmentToN.astChildren.isIdentifier.size shouldBe 1

      val List(fieldAccessN) = assignmentToN.astChildren.isCall.codeExact("_tmp_0.propName").l
      fieldAccessN.name shouldBe Operators.fieldAccess
      fieldAccessN.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      fieldAccessN.astChildren.isFieldIdentifier.canonicalNameExact("propName").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for nested object destruction assignment with defaults as parameter" in {
      val cpg = code("""
       |function userId({id = {}, b} = {}) {
       |  return id;
       |}
       |""".stripMargin)
      val List(userId) = cpg.method.nameExact("userId").l

      val List(param) = userId.parameter.nameExact("param1_0").l
      param.code shouldBe "{id = {}, b} = {}"

      val List(userIdBlock) = userId.astChildren.isBlock.l

      val List(destructionBlock) = userIdBlock.astChildren.isBlock.order(1).l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_1").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_1 = param1_0 === void 0 ? {} : param1_0").size shouldBe 1

      val List(assignmentToId) =
        destructionBlock.astChildren.isCall.codeExact("id = _tmp_1.id === void 0 ? {} : _tmp_1.id").l

      destructionBlock.astChildren.isCall.codeExact("b = _tmp_1.b").size shouldBe 1

      assignmentToId.astChildren.isIdentifier.size shouldBe 1

      val List(ternaryId) = assignmentToId.astChildren.isCall.codeExact("_tmp_1.id === void 0 ? {} : _tmp_1.id").l

      val List(indexAccessId) = ternaryId.astChildren.isCall.codeExact("_tmp_1.id").l
      indexAccessId.astChildren.isIdentifier.nameExact("_tmp_1").size shouldBe 1
      indexAccessId.astChildren.isFieldIdentifier.canonicalNameExact("id").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_1"
    }

    "have correct structure for object destruction assignment as parameter" in {
      val cpg = code("""
      |function userId({id}) {
      |  return id;
      |}
      |""".stripMargin)
      val List(userId)      = cpg.method.nameExact("userId").l
      val List(userIdBlock) = userId.astChildren.isBlock.l
      userIdBlock.astChildren.isLocal.nameExact("id").size shouldBe 1

      val List(assignmentToId) = userIdBlock.astChildren.isCall.codeExact("id = param1_0.id").l
      assignmentToId.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessId) = assignmentToId.astChildren.isCall.codeExact("param1_0.id").l
      indexAccessId.astChildren.isIdentifier.nameExact("param1_0").size shouldBe 1
      indexAccessId.astChildren.isFieldIdentifier.canonicalNameExact("id").size shouldBe 1
    }

    "have correct structure for array destruction assignment with declaration" in {
      val cpg                = code("var [a, b] = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0[0]").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0[0]").l
      indexAccessA.name shouldBe Operators.indexAccess

      indexAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessA.astChildren.codeExact("0").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0[1]").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0[1]").l
      indexAccessB.name shouldBe Operators.indexAccess
      indexAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessB.astChildren.isLiteral.codeExact("1").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for array destruction assignment without declaration" in {
      val cpg                = code("[a, b] = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0[0]").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0[0]").l
      indexAccessA.name shouldBe Operators.indexAccess
      indexAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessA.astChildren.isLiteral.codeExact("0").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0[1]").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0[1]").l
      indexAccessB.name shouldBe Operators.indexAccess
      indexAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessB.astChildren.isLiteral.codeExact("1").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for array destruction assignment with defaults" in {
      val cpg                = code("var [a = 1, b = 2] = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      programBlock.astChildren.isLocal.nameExact("a").size shouldBe 1
      programBlock.astChildren.isLocal.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.assignment.codeExact("a = _tmp_0[0] === void 0 ? 1 : _tmp_0[0]").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(ifA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0[0] === void 0 ? 1 : _tmp_0[0]").l
      ifA.name shouldBe Operators.conditional

      val List(testA) = ifA.astChildren.isCall.codeExact("_tmp_0[0] === void 0").l
      testA.name shouldBe Operators.equals

      val List(testAIndexAccess) = testA.astChildren.isCall.codeExact("_tmp_0[0]").l
      testAIndexAccess.name shouldBe Operators.indexAccess

      testA.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifA.astChildren.isLiteral.codeExact("1").size shouldBe 1

      val List(falseBranchA) = ifA.astChildren.isCall.codeExact("_tmp_0[0]").l
      falseBranchA.name shouldBe Operators.indexAccess

      val List(assignmentToB) = destructionBlock.assignment.codeExact("b = _tmp_0[1] === void 0 ? 2 : _tmp_0[1]").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(ifB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0[1] === void 0 ? 2 : _tmp_0[1]").l
      ifB.name shouldBe Operators.conditional

      val List(testB) = ifB.astChildren.isCall.codeExact("_tmp_0[1] === void 0").l
      testB.name shouldBe Operators.equals

      val List(testBIndexAccess) = testB.astChildren.isCall.codeExact("_tmp_0[1]").l
      testBIndexAccess.name shouldBe Operators.indexAccess

      testB.astChildren.isCall.codeExact("void 0").size shouldBe 1

      ifB.astChildren.isLiteral.codeExact("2").size shouldBe 1

      val List(falseBranchB) = ifB.astChildren.isCall.codeExact("_tmp_0[1]").l

      falseBranchB.name shouldBe Operators.indexAccess

      val List(returnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      returnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for array destruction assignment with ignores" in {
      val cpg                = code("var [a, , b] = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("b").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0[0]").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0[0]").l
      indexAccessA.name shouldBe Operators.indexAccess
      indexAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessA.astChildren.isLiteral.codeExact("0").size shouldBe 1

      val List(assignmentToB) = destructionBlock.astChildren.isCall.codeExact("b = _tmp_0[2]").l
      assignmentToB.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessB) = assignmentToB.astChildren.isCall.codeExact("_tmp_0[2]").l
      indexAccessB.name shouldBe Operators.indexAccess
      indexAccessB.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessB.astChildren.isLiteral.codeExact("2").size shouldBe 1

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"
    }

    "have correct structure for array destruction assignment with rest" in {
      val cpg                = code("var [a, ...rest] = x;")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      cpg.local.nameExact("a").size shouldBe 1
      cpg.local.nameExact("rest").size shouldBe 1

      val List(destructionBlock) = programBlock.astChildren.isBlock.l
      destructionBlock.astChildren.isLocal.nameExact("_tmp_0").size shouldBe 1
      destructionBlock.astChildren.isCall.codeExact("_tmp_0 = x").size shouldBe 1

      val List(assignmentToA) = destructionBlock.astChildren.isCall.codeExact("a = _tmp_0[0]").l
      assignmentToA.astChildren.isIdentifier.size shouldBe 1

      val List(indexAccessA) = assignmentToA.astChildren.isCall.codeExact("_tmp_0[0]").l
      indexAccessA.name shouldBe Operators.indexAccess

      indexAccessA.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      indexAccessA.astChildren.codeExact("0").size shouldBe 1

      val List(restCall) = destructionBlock.astChildren.isCall.nameExact("<operator>.spread").l
      restCall.code shouldBe "...rest"
      val List(tmpArg) = restCall.argument(1).start.isCall.l
      tmpArg.code shouldBe "_tmp_0[1]"
      tmpArg.name shouldBe Operators.indexAccess
      tmpArg.astChildren.isIdentifier.nameExact("_tmp_0").size shouldBe 1
      tmpArg.astChildren.isLiteral.codeExact("1").size shouldBe 1
      val List(restArg) = restCall.argument(2).start.isIdentifier.l
      restArg.code shouldBe "rest"

      val List(tmpReturnIdentifier) = destructionBlock.astChildren.isIdentifier.l
      tmpReturnIdentifier.name shouldBe "_tmp_0"

    }

    "have correct structure for array destruction as parameter" in {
      val cpg = code("""
        |function userId([id]) {
        |  return id;
        |}
        |""".stripMargin)
      val List(userId) = cpg.method.nameExact("userId").l
      userId.parameter.nameExact("param1_0").size shouldBe 1
      val List(userIdBlock) = userId.astChildren.isBlock.l
      userIdBlock.astChildren.isLocal.nameExact("id").size shouldBe 1
      userIdBlock.astChildren.isCall.codeExact("id = param1_0.id").size shouldBe 1
    }

    "have correct structure for method spread argument" in {
      val cpg           = code("foo(...args)")
      val List(fooCall) = cpg.call.codeExact("foo(...args)").l
      fooCall.name shouldBe "foo"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = fooCall.receiver.isIdentifier.l
      receiver.name shouldBe "foo"
      receiver.argumentIndex shouldBe -1

      val List(argumentThis) = fooCall.astChildren.isIdentifier.nameExact("this").l
      argumentThis.argumentIndex shouldBe 0

      val List(argument1) = fooCall.astChildren.isCall.nameExact("<operator>.spread").l
      argument1.argumentIndex shouldBe 1
      argument1.code shouldBe "...args"
      argument1.argument(1).code shouldBe "args"
    }

    "have correct structure for complex method spread argument" in {
      val cpg           = code("foo(...x.bar());")
      val List(fooCall) = cpg.call.codeExact("foo(...x.bar())").l
      fooCall.name shouldBe "foo"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = fooCall.receiver.isIdentifier.l
      receiver.name shouldBe "foo"
      receiver.argumentIndex shouldBe -1

      val List(argumentThis) = fooCall.astChildren.isIdentifier.nameExact("this").l
      argumentThis.argumentIndex shouldBe 0

      val List(argument1) = fooCall.astChildren.isCall.nameExact("<operator>.spread").l
      argument1.argumentIndex shouldBe 1
      argument1.code shouldBe "...x.bar()"
      val List(arg) = argument1.argument.isCall.l
      arg.code shouldBe "x.bar()"
      arg.argumentIndex shouldBe 1
    }
  }

  "AST generation for await/async" should {
    "have correct structure for await/async" in {
      val cpg             = code("async function x(foo) { await foo() }")
      val List(awaitCall) = cpg.method.nameExact("x").astChildren.isBlock.astChildren.isCall.l
      awaitCall.code shouldBe "await foo()"
      awaitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      awaitCall.methodFullName shouldBe "<operator>.await"
      awaitCall.astChildren.isCall.codeExact("foo()").size shouldBe 1
    }
  }

  "AST generation for instanceof/delete" should {
    "have correct structure for instanceof" in {
      val cpg           = code("x instanceof Foo;")
      val List(program) = cpg.method.nameExact(":program").l

      val List(instanceOf) = program.astChildren.isBlock.astChildren.isCall.codeExact("x instanceof Foo").l
      instanceOf.name shouldBe Operators.instanceOf

      val List(lhs) = instanceOf.astChildren.isIdentifier.nameExact("x").l
      lhs.code shouldBe "x"
      val List(lhsArg) = instanceOf.argument.isIdentifier.nameExact("x").l
      lhsArg.code shouldBe "x"

      val List(rhs) = instanceOf.astChildren.isIdentifier.nameExact("Foo").l
      rhs.code shouldBe "Foo"
      val List(rhsArg) = instanceOf.argument.isIdentifier.nameExact("Foo").l
      rhsArg.code shouldBe "Foo"
    }

    "have correct structure for delete" in {
      val cpg           = code("delete foo.x;")
      val List(program) = cpg.method.nameExact(":program").l

      val List(delete) = program.astChildren.isBlock.astChildren.isCall.codeExact("delete foo.x").l
      delete.name shouldBe Operators.delete

      val List(rhs) = delete.fieldAccess.l
      rhs.code shouldBe "foo.x"
    }
  }

  "AST generation for default parameters" should {
    "have correct structure for method parameter with default" in {
      val cpg       = code("function foo(a = 1) {}")
      val List(foo) = cpg.method.nameExact("foo").l

      val List(paramA) = foo.parameter.nameExact("a").l
      paramA.index shouldBe 1

      val List(block)      = foo.astChildren.isBlock.l
      val List(assignment) = block.astChildren.isCall.l
      assignment.astChildren.isIdentifier.nameExact("a").size shouldBe 1

      val List(ternaryCall) = assignment.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCall)    = ternaryCall.astChildren.isCall.nameExact(Operators.equals).l
      testCall.astChildren.isIdentifier.nameExact("a").size shouldBe 1
      testCall.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCall.astChildren.isLiteral.codeExact("1").size shouldBe 1
      ternaryCall.astChildren.isIdentifier.nameExact("a").size shouldBe 1
    }

    "have correct structure for multiple method parameters with default" in {
      val cpg          = code("function foo(a = 1, b = 2) {}")
      val List(foo)    = cpg.method.nameExact("foo").l
      val List(paramA) = foo.parameter.nameExact("a").l
      paramA.index shouldBe 1

      val List(paramB) = foo.parameter.nameExact("b").l
      paramB.index shouldBe 2

      val List(block) = foo.astChildren.isBlock.l

      val List(assignmentA) = block.astChildren.isCall.codeExact("a = a === void 0 ? 1 : a").l
      assignmentA.astChildren.isIdentifier.nameExact("a").size shouldBe 1

      val List(ternaryCallA) = assignmentA.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCallA) =
        ternaryCallA.astChildren.isCall.nameExact(Operators.equals).l
      testCallA.astChildren.isIdentifier.nameExact("a").size shouldBe 1
      testCallA.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCallA.astChildren.isLiteral.codeExact("1").size shouldBe 1
      ternaryCallA.astChildren.isIdentifier.nameExact("a").size shouldBe 1

      val List(assignmentB) = block.astChildren.isCall.codeExact("b = b === void 0 ? 2 : b").l
      assignmentB.astChildren.isIdentifier.nameExact("b").size shouldBe 1

      val List(ternaryCallB) = assignmentB.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCallB)    = ternaryCallB.astChildren.isCall.nameExact(Operators.equals).l
      testCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1
      testCallB.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCallB.astChildren.isLiteral.codeExact("2").size shouldBe 1
      ternaryCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1
    }

    "have correct structure for method mixed parameters with default" in {
      val cpg          = code("function foo(a, b = 1) {}")
      val List(foo)    = cpg.method.nameExact("foo").l
      val List(paramA) = foo.parameter.nameExact("a").l
      paramA.index shouldBe 1
      val List(paramB) = foo.parameter.nameExact("b").l
      paramB.index shouldBe 2

      val List(block) = foo.astChildren.isBlock.l

      val List(assignmentB) = block.astChildren.isCall.codeExact("b = b === void 0 ? 1 : b").l
      assignmentB.astChildren.isIdentifier.nameExact("b").size shouldBe 1

      val List(ternaryCallB) = assignmentB.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCallB)    = ternaryCallB.astChildren.isCall.nameExact(Operators.equals).l
      testCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1
      testCallB.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCallB.astChildren.isLiteral.codeExact("1").size shouldBe 1
      ternaryCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1
    }

    "have correct structure for multiple method mixed parameters with default" in {
      val cpg          = code("function foo(a, b = 1, c = 2) {}")
      val List(foo)    = cpg.method.nameExact("foo").l
      val List(paramA) = foo.parameter.nameExact("a").l
      paramA.index shouldBe 1
      val List(paramB) = foo.parameter.nameExact("b").l
      paramB.index shouldBe 2
      val List(paramC) = foo.parameter.nameExact("c").l
      paramC.index shouldBe 3

      val List(block) = foo.astChildren.isBlock.l

      val List(assignmentB) = block.astChildren.isCall.codeExact("b = b === void 0 ? 1 : b").l
      assignmentB.astChildren.isIdentifier.nameExact("b").size shouldBe 1

      val List(ternaryCallB) = assignmentB.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCallB)    = ternaryCallB.astChildren.isCall.nameExact(Operators.equals).l
      testCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1
      testCallB.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCallB.astChildren.isLiteral.codeExact("1").size shouldBe 1
      ternaryCallB.astChildren.isIdentifier.nameExact("b").size shouldBe 1

      val List(assignmentC) = block.astChildren.isCall.codeExact("c = c === void 0 ? 2 : c").l
      assignmentC.astChildren.isIdentifier.nameExact("c").size shouldBe 1

      val List(ternaryCallC) = assignmentC.astChildren.isCall.nameExact(Operators.conditional).l
      val List(testCallC)    = ternaryCallC.astChildren.isCall.nameExact(Operators.equals).l
      testCallC.astChildren.isIdentifier.nameExact("c").size shouldBe 1
      testCallC.astChildren.isCall.nameExact("<operator>.void").size shouldBe 1
      ternaryCallC.astChildren.isLiteral.codeExact("2").size shouldBe 1
      ternaryCallC.astChildren.isIdentifier.nameExact("c").size shouldBe 1
    }
  }

  "AST generation for import/require" should {
    "make available `import` statements via cpg.imports" in {
      val cpg       = code("import {x} from \"foo\";")
      val List(imp) = cpg.imports.l
      imp.code shouldBe "import {x} from \"foo\""
      imp.importedEntity shouldBe Option("foo:x")
      imp.importedAs shouldBe Option("x")
    }

    "allow traversing from dependency to import from for `import` statements" in {
      val cpg        = code("import {x} from \"foo\";")
      val List(imp)  = cpg.imports.l
      val List(dep)  = cpg.dependency.l
      val List(imp2) = dep.imports.l
      imp shouldBe imp2
    }

    "make available `require` statements via cpg.imports" in {
      val cpg       = code("const x = require(\"foo\");")
      val List(imp) = cpg.imports.l
      imp.code shouldBe "x = require(\"foo\")"
      imp.importedEntity shouldBe Option("foo")
      imp.importedAs shouldBe Option("x")
    }

    "allow traversing from dependency to import for `require` statements" in {
      val cpg        = code("const x = require(\"foo\");")
      val List(imp)  = cpg.imports.l
      val List(dep)  = cpg.dependency.l
      val List(imp2) = dep.imports.l
      imp shouldBe imp2
    }
  }

}
