package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTests extends AstJsSrc2CpgSuite {

  "AST generation for simple fragments" should {

    "contain the correct file nodes" in {
      val cpg            = code("")
      val List(fileTest) = cpg.file.l
      fileTest.name shouldBe "Test0.js"
      fileTest.order shouldBe 0
    }

    "have correct structure for with statement with block" in {
      val cpg = code("""
        |with(foo()) {
        |  bar();
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(withBlock)   = methodBlock.astChildren.isBlock.l
      withBlock.astChildren.isCall.code.l shouldBe List("foo()", "bar()")
    }

    "have correct structure for with statement without block" in {
      val cpg = code("""
        |with(foo())
        |  bar();
        |baz();
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      methodBlock.astChildren.isCall.code.l shouldBe List("baz()")
      val List(withBlock) = methodBlock.astChildren.isBlock.l
      withBlock.astChildren.isCall.code.l shouldBe List("foo()", "bar()")
    }

    "have correct structure for long numeric literal" in {
      val cpg       = code("console.log(1e20)")
      val List(lit) = cpg.literal.l
      lit.code shouldBe "1e20"
    }

    "have correct structure for non null expression" in {
      val cpg               = code("const foo = bar!")
      val List(nonNullCall) = cpg.call(Operators.notNullAssert).l
      val List(arg)         = nonNullCall.argument.isIdentifier.l
      arg.name shouldBe "bar"
      arg.code shouldBe "bar"
    }

    "have return node for arrow functions" in {
      val cpg = code("const foo = () => 42;")
      // Return node is necessary data flow
      val methodBlock = cpg.method("<lambda>0").astChildren.isBlock
      val literal     = methodBlock.astChildren.isReturn.astChildren.isLiteral.head
      literal.code shouldBe "42"
    }

    "have only 1 Block Node for arrow functions" in {
      val cpg = code("const foo = () => {return 42;}")
      cpg.method("<lambda>0").ast.isBlock.size shouldBe 1
    }

    "have correct structure for FILENAME property" in {
      val cpg = code("let x = 1;")
      cpg.namespaceBlock.filenameExact("Test0.js").size shouldBe 1

      val List(program) = cpg.method.nameExact(":program").l
      program.filename shouldBe "Test0.js"

      val List(typeDecls) = cpg.typeDecl.nameExact(":program").l
      typeDecls.filename shouldBe "Test0.js"
    }

    "have correct type for literals" in {
      val cpg           = code("let x = 1; let y = 'y'; let z = false;")
      val List(x, y, z) = cpg.literal.l
      x.typeFullName shouldBe Defines.Number
      y.typeFullName shouldBe Defines.String
      z.typeFullName shouldBe Defines.Boolean
    }

    "have correct structure for multiple declarators in one place" in {
      val cpg                                         = code("let x = 1, y = 2, z = 3;")
      val List(xAssignment, yAssignment, zAssignment) = cpg.call.l.sortBy(_.code)
      xAssignment.code shouldBe "let x = 1"
      yAssignment.code shouldBe "let y = 2"
      zAssignment.code shouldBe "let z = 3"

      val List(program) = cpg.method.nameExact(":program").l
      program.ast.isCall.l.sortBy(_.code) shouldBe cpg.call.l.sortBy(_.code)
    }

    "have correct structure for call on require" in {
      val cpg                                 = code("var x = require(\"foo\").bar;")
      val List(reqCall, barCall, xAssignment) = cpg.call.l.sortBy(_.code)
      reqCall.code shouldBe "require(\"foo\")"
      barCall.code shouldBe "require(\"foo\").bar"
      xAssignment.code shouldBe "var x = require(\"foo\").bar"

      val List(program) = cpg.method.nameExact(":program").l
      program.ast.isCall.l.sortBy(_.code) shouldBe cpg.call.l.sortBy(_.code)
    }

    "have correct structure for block expression" in {
      val cpg           = code("let x = (class Foo {}, bar())")
      val List(program) = cpg.method.nameExact(":program").l

      val List(classFooTypeDecl) = cpg.typeDecl.nameExact("Foo").l
      classFooTypeDecl.fullName shouldBe "Test0.js::program:Foo"

      // constructor
      val List(classFooMethod) =
        classFooTypeDecl.astChildren.isMethod.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l
      classFooMethod.code shouldBe "constructor() {}"

      val List(programBlock) = program.astChildren.isBlock.l

      val List(assignment) = programBlock.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(commaRight) = assignment.astChildren.isBlock.l

      val List(refForConstructor) = commaRight.astChildren.isTypeRef.l
      refForConstructor.code shouldBe "class Foo"

      val List(barCall) = commaRight.astChildren.isCall.l
      barCall.code shouldBe "bar()"
    }

    "have correct structure for index access" in {
      val cpg                   = code("if(d = decorators[i]) foo();")
      val List(indexAccessCall) = cpg.call(Operators.indexAccess).l
      indexAccessCall.code shouldBe "decorators[i]"
      val List(baseArg, indexArg) = indexAccessCall.argument.isIdentifier.l
      baseArg.name shouldBe "decorators"
      baseArg.argumentIndex shouldBe 1
      indexArg.name shouldBe "i"
      indexArg.argumentIndex shouldBe 2
    }

    "have correct structure for empty array literal" in {
      val cpg               = code("var x = []")
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(xAssignment) = methodBlock.astChildren.isCall.l
      xAssignment.name shouldBe Operators.assignment

      val List(arrayCall) = xAssignment.astChildren.isCall.l
      arrayCall.name shouldBe EcmaBuiltins.arrayFactory
      arrayCall.code shouldBe s"${EcmaBuiltins.arrayFactory}()"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "have correct structure for array literal with values" in {
      val cpg               = code("var x = [1, 2]")
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l

      val List(xAssignment) = methodBlock.astChildren.isCall.l
      xAssignment.name shouldBe Operators.assignment

      val List(pushBlock) = xAssignment.astChildren.isBlock.l

      val List(tmpLocal) = pushBlock.astChildren.isLocal.l
      tmpLocal.name shouldBe "_tmp_0"

      val List(tmpAssignment) = pushBlock.astChildren.isCall.codeExact("_tmp_0 = __ecma.Array.factory()").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(arrayCall) = tmpAssignment.astChildren.isCall.l
      arrayCall.name shouldBe EcmaBuiltins.arrayFactory
      arrayCall.code shouldBe s"${EcmaBuiltins.arrayFactory}()"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      checkLiterals(pushBlock, 1)
      checkLiterals(pushBlock, 2)

      val List(tmpReturn) = pushBlock.astChildren.isIdentifier.l
      tmpReturn.name shouldBe "_tmp_0"
    }

    "have correct structure for array literal with too many values" in {
      val cpg               = code(s"var x = [1, 2, ${("n" * 1500).mkString(",")}]")
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l

      val List(xAssignment) = methodBlock.astChildren.isCall.l
      xAssignment.name shouldBe Operators.assignment

      val List(pushBlock) = xAssignment.astChildren.isBlock.l

      val List(tmpLocal) = pushBlock.astChildren.isLocal.l
      tmpLocal.name shouldBe "_tmp_0"

      val List(tmpAssignment) = pushBlock.astChildren.isCall.codeExact("_tmp_0 = __ecma.Array.factory()").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(arrayCall) = tmpAssignment.astChildren.isCall.l
      arrayCall.name shouldBe EcmaBuiltins.arrayFactory
      arrayCall.code shouldBe s"${EcmaBuiltins.arrayFactory}()"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      checkLiterals(pushBlock, 1)
      checkLiterals(pushBlock, 2)

      // all other elements are truncated
      val List(literalNode) = pushBlock.astChildren.isLiteral.l
      literalNode.code shouldBe "<too-many-initializers>"
      literalNode.argumentIndex shouldBe 1002

      val List(tmpReturn) = pushBlock.astChildren.isIdentifier.l
      tmpReturn.name shouldBe "_tmp_0"
    }

    "have correct structure for untagged runtime node in call" in {
      val cpg               = code(s"foo(`Hello $${world}!`)")
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(fooCall) = methodBlock.astChildren.isCall.l
      fooCall.code shouldBe s"foo(`Hello $${world}!`)"

      val List(templateCall) = fooCall.astChildren.isCall.l
      templateCall.name shouldBe Operators.formatString
      templateCall.code shouldBe s"${Operators.formatString}(\"Hello \", world, \"!\")"

      val List(argument1) = templateCall.astChildren.isLiteral.order(1).l
      argument1.argumentIndex shouldBe 1
      argument1.code shouldBe "\"Hello \""

      val List(argument2) = templateCall.astChildren.isIdentifier.order(2).l
      argument2.argumentIndex shouldBe 2
      argument2.name shouldBe "world"
      argument2.code shouldBe "world"

      val List(argument3) = templateCall.astChildren.isLiteral.order(3).l
      argument3.argumentIndex shouldBe 3
      argument3.code shouldBe "\"!\""
    }

    "have correct structure for untagged runtime node" in {
      val cpg               = code(s"`$${x + 1}`")
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(call) = methodBlock.astChildren.isCall.l
      call.name shouldBe Operators.formatString
      call.code shouldBe s"${Operators.formatString}(\"\", x + 1, \"\")"

      val List(argument1) = call.astChildren.isLiteral.order(1).l
      argument1.argumentIndex shouldBe 1
      argument1.code shouldBe "\"\""

      val List(argument2) = call.astChildren.isCall.order(2).l
      argument2.argumentIndex shouldBe 2
      argument2.code shouldBe "x + 1"

      val List(argument3) = call.astChildren.isLiteral.order(3).l
      argument3.argumentIndex shouldBe 3
      argument3.code shouldBe "\"\""
    }

    "have correct structure for tagged runtime node" in {
      val cpg               = code(s"String.raw`../$${42}\\..`")
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(rawCall) = methodBlock.astChildren.isCall.l
      rawCall.code shouldBe s"String.raw(${Operators.formatString}(\"../\", 42, \"\\..\"))"

      val List(runtimeCall) = rawCall.astChildren.isCall.nameExact(Operators.formatString).l
      runtimeCall.order shouldBe 1
      runtimeCall.argumentIndex shouldBe 1
      runtimeCall.code shouldBe s"${Operators.formatString}(\"../\", 42, \"\\..\")"

      val List(argument1) = runtimeCall.astChildren.isLiteral.codeExact("\"../\"").l
      argument1.order shouldBe 1
      argument1.argumentIndex shouldBe 1

      val List(argument2) = runtimeCall.astChildren.isLiteral.codeExact("42").l
      argument2.order shouldBe 2
      argument2.argumentIndex shouldBe 2

      val List(argument3) =
        runtimeCall.astChildren.isLiteral.codeExact("\"\\..\"").l
      argument3.order shouldBe 3
      argument3.argumentIndex shouldBe 3
    }

    "have correct structure for different string literals" in {
      val cpg = code("""
        |var keyA = "AAA";
        |var keyB = 'BBB';
        |var keyC = `CCC`;
        |var keyD = `DDD"`;
        |var keyE = "EE EE E";
        |var keyF = "F-FF-F";
        |""".stripMargin)
      cpg.literal.code.l shouldBe List(
        """"AAA"""",
        """"BBB"""",
        """"CCC"""",
        """"DDD""""",
        """"EE EE E"""",
        """"F-FF-F""""
      )
      cpg.call.code.l shouldBe List(
        """var keyA = "AAA"""",
        """var keyB = 'BBB'""",
        """var keyC = `CCC`""",
        """var keyD = `DDD"`""",
        """var keyE = "EE EE E"""",
        """var keyF = "F-FF-F""""
      )
    }

    "have correct structure for try" in {
      val cpg = code("""
       |try {
       | open()
       |} catch(err) {
       | handle()
       |} finally {
       | close()
       |}
       |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(tryStatement) = methodBlock.astChildren.isControlStructure.l
      tryStatement.controlStructureType shouldBe ControlStructureTypes.TRY

      val List(tryBlock) = tryStatement.astChildren.isBlock.order(1).l
      tryBlock.ast.isCall.codeExact("open()").size shouldBe 1

      val List(catchBlock) = tryStatement.astChildren.isControlStructure.isCatch.l
      catchBlock.order shouldBe 2
      catchBlock.ast.isCall.codeExact("handle()").size shouldBe 1

      val List(finallyBlock) = tryStatement.astChildren.isControlStructure.isFinally.l
      finallyBlock.order shouldBe 3
      finallyBlock.ast.isCall.codeExact("close()").size shouldBe 1
    }

    "have correct structure for try with empty catch / finally" in {
      val cpg = code("""
          |try {
          | open()
          |} catch(err) {}
          |finally {}
          |""".stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(tryStatement) = methodBlock.astChildren.isControlStructure.l
      tryStatement.controlStructureType shouldBe ControlStructureTypes.TRY

      val List(tryBlock) = tryStatement.astChildren.isBlock.order(1).l
      tryBlock.ast.isCall.codeExact("open()").size shouldBe 1

      val List(catchBlock) = tryStatement.astChildren.isControlStructure.isCatch.l
      catchBlock.order shouldBe 2
      catchBlock.astChildren.astChildren.code.l shouldBe List("err")

      val List(finallyBlock) = tryStatement.astChildren.isControlStructure.isFinally.l
      finallyBlock.order shouldBe 3
      finallyBlock.astChildren.astChildren shouldBe empty
    }

    "have correct structure for 1 object with simple values" in {
      val cpg = code("""
       |var x = {
       | key1: "value",
       | key2: 2,
       | ...rest
       |}
       |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("key1", "\"value\""))
      checkObjectInitialization(block, ("key2", "2"))

      val List(spreadObjectCall) = block.astChildren.isCall.nameExact("<operator>.spread").l
      spreadObjectCall.code shouldBe "...rest"
      val List(tmpArg: Identifier, restArg) = spreadObjectCall.argument.isIdentifier.l
      tmpArg.code shouldBe "_tmp_0"
      tmpArg.name shouldBe "_tmp_0"
      tmpArg.argumentIndex shouldBe 1
      restArg.code shouldBe "rest"
      restArg.name shouldBe "rest"
      restArg.argumentIndex shouldBe 2
    }

    "have correct structure for 1 object with complex rest" in {
      val cpg = code("""
       |var x = {
       | key1: "value",
       | key2: 2,
       | ...x.foo()
       |}
       |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("key1", "\"value\""))
      checkObjectInitialization(block, ("key2", "2"))

      val List(spreadObjectCall) = block.astChildren.isCall.nameExact("<operator>.spread").l
      spreadObjectCall.code shouldBe "...x.foo()"
      val List(tmpArg) = spreadObjectCall.argument.isIdentifier.l
      tmpArg.code shouldBe "_tmp_0"
      tmpArg.name shouldBe "_tmp_0"
      tmpArg.argumentIndex shouldBe 1
      val List(restArg) = spreadObjectCall.argument.isCall.l
      restArg.code shouldBe "x.foo()"
      restArg.argumentIndex shouldBe 2
    }

    "have correct structure for 1 object with computed values" in {
      val cpg = code("""
       |var x = {
       | key1: value(),
       | key2: foo.compute()
       |}
       |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("key1", "value()"))
      checkObjectInitialization(block, ("key2", "foo.compute()"))
    }

    "have correct structure for 1 object with object function" in {
      val cpg = code("""
       |var x = {
       | key1: value(),
       | foo() {},
       | 'bladad'() {}
       |}
       |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("key1", "value()"))
      checkObjectInitialization(block, ("foo", "foo")) // ref to foo
    }

    "have correct structure for object with computed property name" in {
      val cpg = code("""
      |var x = {
      | [ 1 + 1 ]: value()
      |}""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("_computed_object_property_0", "value()"))
    }

    "have correct structure for object with property names with quotes" in {
      val cpg = code("""var x = {
        | "a": 1,
        | 'b': 2
        |}""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact(":program").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("a", "1"))
      checkObjectInitialization(block, ("b", "2"))
    }

    "have correct structure for conditional expression" in {
      val cpg           = code("x ? y : z;")
      val List(program) = cpg.method.nameExact(":program").l
      val List(block)   = program.astChildren.isBlock.l
      val List(call)    = block.astChildren.isCall.l
      call.code shouldBe "x ? y : z"
      call.methodFullName shouldBe Operators.conditional

      val List(x, y, z) = call.astChildren.isIdentifier.l
      x.name shouldBe "x"
      y.name shouldBe "y"
      z.name shouldBe "z"
    }

    "have correct file name for empty file" in {
      val cpg        = code("function method(x) {}")
      val List(file) = cpg.file.l
      file.name should endWith("Test0.js")
    }

    "have correct name space block for empty file" in {
      val cpg        = code("")
      val List(file) = cpg.file.l
      file.name should endWith("Test0.js")

      val List(ns) = cpg.namespaceBlock.l
      ns.name shouldBe Defines.GlobalNamespace
      ns.fullName should endWith(s"Test0.js:${Defines.GlobalNamespace}")
      ns.order shouldBe 1
      ns.filename shouldBe file.name
    }

    "have :program method correctly attached to files namespace block" in {
      val cpg        = code("")
      val List(file) = cpg.file.l
      file.name should endWith("Test0.js")

      val List(ns) = cpg.namespaceBlock.l
      ns.name shouldBe Defines.GlobalNamespace
      ns.fullName should endWith(s"Test0.js:${Defines.GlobalNamespace}")
      ns.order shouldBe 1
      ns.typeDecl.nameExact(":program").method.head.name shouldBe ":program"
    }

    "have correct structure for empty method nested in top level method" in {
      val cpg                = code("function method(x) {}")
      val List(method)       = cpg.method.nameExact(":program").l
      val List(methodMethod) = method.astChildren.isMethod.l

      val List(virtualModifier) = methodMethod.modifier.l
      virtualModifier.modifierType shouldBe ModifierTypes.VIRTUAL

      val List(block) = method.astChildren.isBlock.l

      val List(assignment) = block.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(localForMethod) = block.astChildren.isLocal.l
      localForMethod.name shouldBe "method"

      val List(methodIdentifier) = assignment.astChildren.isIdentifier.argumentIndex(1).l
      methodIdentifier.name shouldBe "method"

      methodIdentifier.refOut.head shouldBe localForMethod
    }

    "have correct parameter order in lambda function with ignored param" in {
      val cpg               = code("var x = ([, param]) => param")
      val lambdaFullName    = "Test0.js::program:<lambda>0"
      val List(lambda)      = cpg.method.fullNameExact(lambdaFullName).isLambda.l
      val List(lambdaBlock) = lambda.astChildren.isBlock.l

      val List(param1, param2) = lambda.parameter.l
      param1.index shouldBe 0
      param1.name shouldBe "this"
      param1.code shouldBe "this"

      param2.index shouldBe 1
      param2.name shouldBe "param1_0"
      param2.code shouldBe "[, param]"

      lambdaBlock.astChildren.isLocal.nameExact("param").size shouldBe 1
      lambdaBlock.astChildren.isCall.codeExact("param = param1_0.param").size shouldBe 1
    }

    "have correct parameter in lambda function rest param in object" in {
      val cpg               = code("var x = ({x, ...rest}) => x + rest")
      val lambdaFullName    = "Test0.js::program:<lambda>0"
      val List(lambda)      = cpg.method.fullNameExact(lambdaFullName).isLambda.l
      val List(lambdaBlock) = lambda.astChildren.isBlock.l

      val List(param1, param2) = lambda.parameter.l
      param1.index shouldBe 0
      param1.name shouldBe "this"
      param1.code shouldBe "this"
      param1.typeFullName shouldBe Defines.Any
      param1.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")

      param2.index shouldBe 1
      param2.name shouldBe "param1_0"
      param2.code shouldBe "{x, ...rest}"

      lambdaBlock.astChildren.isLocal.nameExact("x").size shouldBe 1
      lambdaBlock.astChildren.isLocal.nameExact("rest").size shouldBe 1
      lambdaBlock.astChildren.isCall.codeExact("rest = param1_0.rest").size shouldBe 1
    }

    "have correct parameter in lambda function rest param in array" in {
      val cpg               = code("var x = ([x, ...rest]) => x + rest")
      val lambdaFullName    = "Test0.js::program:<lambda>0"
      val List(lambda)      = cpg.method.fullNameExact(lambdaFullName).isLambda.l
      val List(lambdaBlock) = lambda.astChildren.isBlock.l

      val List(param1, param2) = lambda.parameter.l
      param1.index shouldBe 0
      param1.name shouldBe "this"
      param1.code shouldBe "this"
      param1.typeFullName shouldBe Defines.Any
      param1.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")

      param2.index shouldBe 1
      param2.name shouldBe "param1_0"
      param2.code shouldBe "[x, ...rest]"

      lambdaBlock.astChildren.isLocal.nameExact("x").size shouldBe 1
      lambdaBlock.astChildren.isLocal.nameExact("rest").size shouldBe 1
      lambdaBlock.astChildren.isCall.codeExact("rest = param1_0.rest").size shouldBe 1
    }

    "have two lambda functions in same scope level with different full names" in {
      val cpg = code("""
        |var x = (a) => a;
        |var y = (b) => b;""".stripMargin)
      val lambda1FullName = "Test0.js::program:<lambda>0"
      val lambda2FullName = "Test0.js::program:<lambda>1"

      cpg.method.fullNameExact(lambda1FullName).size shouldBe 1
      cpg.method.fullNameExact(lambda2FullName).size shouldBe 1

      val List(method) = cpg.method.nameExact(":program").l
      val List(block)  = method.astChildren.isBlock.l

      val List(assignment1) = block.astChildren.isCall.order(1).l
      assignment1.name shouldBe Operators.assignment

      val List(lambda1MethodRef) = assignment1.astChildren.isMethodRef.l
      lambda1MethodRef.methodFullName shouldBe lambda1FullName

      val List(assignment2) = block.astChildren.isCall.order(2).l
      assignment2.name shouldBe Operators.assignment

      val List(lambda2MethodRef) = assignment2.astChildren.isMethodRef.l
      lambda2MethodRef.methodFullName shouldBe lambda2FullName
    }

    "be correct for lambdas returning lambdas" in {
      val cpg = code("() => async () => { }")
      cpg.method.fullName.sorted.l shouldBe List(
        "Test0.js::program",
        "Test0.js::program:<lambda>0",
        "Test0.js::program:<lambda>0:<lambda>1"
      )
      val List(ret) = cpg.method.fullNameExact("Test0.js::program:<lambda>0").block.astChildren.isReturn.l
      ret.code shouldBe "async () => { }"
      val List(ref) = ret.astChildren.isMethodRef.l
      ref.methodFullName shouldBe "Test0.js::program:<lambda>0:<lambda>1"
    }

    "be correct for ThisExpression" in {
      val cpg                  = code("function foo() { this.bar = 1 }")
      val List(thisIdentifier) = cpg.fieldAccess.argument.isIdentifier.l
      thisIdentifier.name shouldBe "this"
      thisIdentifier.code shouldBe "this"
      thisIdentifier.argumentIndex shouldBe 1
      thisIdentifier.typeFullName shouldBe Defines.Any
      thisIdentifier.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")

      val List(thisParameter) = cpg.method.name("foo").parameter.l
      thisParameter.name shouldBe "this"
      thisParameter.code shouldBe "this"

      val referencingIdentifiers = cpg.method.name("foo").parameter.name("this").referencingIdentifiers.l
      referencingIdentifiers shouldBe List(thisIdentifier)
    }

    "be correct for call expression" in {
      val cpg = code("""
         |function method(x) {
         |  foo(x);
         |}
        """.stripMargin)
      val List(method) = cpg.method.nameExact("method").l
      val List(block)  = method.astChildren.isBlock.l

      val List(fooCall) = block.astChildren.isCall.l
      fooCall.code shouldBe "foo(x)"
      fooCall.name shouldBe "foo"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = fooCall.receiver.isIdentifier.l
      receiver.name shouldBe "foo"
      receiver.argumentIndex shouldBe -1

      val List(argumentThis) = fooCall.astChildren.isIdentifier.nameExact("this").l
      argumentThis.argumentIndex shouldBe 0

      val List(argument1) = fooCall.astChildren.isIdentifier.nameExact("x").l
      argument1.argumentIndex shouldBe 1
    }

    "be correct for chained calls" in {
      val cpg          = code("x.foo(y).bar(z)")
      val List(method) = cpg.method.nameExact(":program").l
      val List(block)  = method.astChildren.isBlock.l

      val List(barCall) = block.astChildren.isCall.l
      barCall.code shouldBe "x.foo(y).bar(z)"
      barCall.name shouldBe "bar"

      val List(receiver)       = barCall.receiver.isCall.l
      val List(receiverViaAst) = barCall.astChildren.isCall.l

      receiver shouldBe receiverViaAst
      receiver.code shouldBe "(_tmp_0 = x.foo(y)).bar"
      receiver.name shouldBe Operators.fieldAccess
      receiver.argumentIndex shouldBe -1

      val List(barIdentifier) = receiver.astChildren.isFieldIdentifier.l
      barIdentifier.canonicalName shouldBe "bar"
      barIdentifier.argumentIndex shouldBe 2

      val List(tmpAssignment) = receiver.astChildren.isCall.l
      tmpAssignment.code shouldBe "(_tmp_0 = x.foo(y))"
      tmpAssignment.name shouldBe "<operator>.assignment"

      val List(tmpIdentifier) = tmpAssignment.astChildren.isIdentifier.l
      tmpIdentifier.name shouldBe "_tmp_0"
      tmpIdentifier.argumentIndex shouldBe 1

      val List(barBaseTree) = tmpAssignment.astChildren.isCall.l
      barBaseTree.code shouldBe "x.foo(y)"
      barBaseTree.name shouldBe "foo"
      barBaseTree.argumentIndex shouldBe 2

      // barBaseTree constructs is tested for in another test.

      val List(thisArg) = barCall.astChildren.isIdentifier.argumentIndex(0).l
      thisArg.name shouldBe "_tmp_0"

      val List(zArg) = barCall.astChildren.isIdentifier.argumentIndex(1).l
      zArg.name shouldBe "z"
    }

    "be correct for call on object" in {
      val cpg = code("""
          |function method(x) {
          |  x.foo();
          |}
        """.stripMargin)
      val List(method) = cpg.method.nameExact("method").l
      val List(block)  = method.astChildren.isBlock.l

      val List(fooCall) = block.astChildren.isCall.l
      fooCall.code shouldBe "x.foo()"
      fooCall.name shouldBe "foo"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = fooCall.astChildren.isCall.l
      receiver.code shouldBe "x.foo"
      receiver.methodFullName shouldBe Operators.fieldAccess

      val List(base) = receiver.astChildren.isIdentifier.argumentIndex(1).l
      base.name shouldBe "x"

      val List(accessElement) = receiver.astChildren.isFieldIdentifier.argumentIndex(2).l
      accessElement.canonicalName shouldBe "foo"
    }

    "be correct for call on object with argument" in {
      val cpg = code("""
        |function method(x) {
        |  a.b(x);
        |}
        """.stripMargin)
      val List(method) = cpg.method.nameExact("method").l
      val List(block)  = method.astChildren.isBlock.l

      val List(call) = block.astChildren.isCall.l
      call.code shouldBe "a.b(x)"
      call.name shouldBe "b"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = call.receiver.isCall.l
      receiver.code shouldBe "a.b"
      receiver.methodFullName shouldBe Operators.fieldAccess

      val List(base) = receiver.astChildren.isIdentifier.argumentIndex(1).l
      base.name shouldBe "a"

      val List(accessElement) = receiver.astChildren.isFieldIdentifier.argumentIndex(2).l
      accessElement.canonicalName shouldBe "b"

      val List(baseArg, arg) = call.argument.l.sortBy(_.order)
      baseArg.code shouldBe "a"
      baseArg.argumentIndex shouldBe 0
      arg.code shouldBe "x"
      arg.argumentIndex shouldBe 1
    }

    "have block for while body for while statement with brackets" in {
      val cpg             = code("while (x < 0) {}")
      val List(method)    = cpg.method.nameExact(":program").l
      val List(block)     = method.astChildren.isBlock.l
      val List(whileNode) = block.astChildren.isControlStructure.l
      whileNode.controlStructureType shouldBe ControlStructureTypes.WHILE
      whileNode.astChildren.isBlock.size shouldBe 1
    }

    "have no block for while body for while statement without brackets" in {
      val cpg = code("""
          |while (x < 0)
          |  x += 1
        """.stripMargin)
      val List(method) = cpg.method.nameExact(":program").l
      val List(block)  = method.astChildren.isBlock.l

      val List(whileNode) = block.astChildren.isControlStructure.l
      whileNode.controlStructureType shouldBe ControlStructureTypes.WHILE
      whileNode.astChildren.isBlock.size shouldBe 0
    }

    "have local variable for function with correct type full name" in {
      val cpg          = code("function method(x) {}")
      val List(method) = cpg.method.nameExact(":program").l
      val block        = method.block
      val localFoo     = block.local.head
      localFoo.name shouldBe "method"
      localFoo.typeFullName should endWith("Test0.js::program:method")
    }

    "have corresponding type decl with correct bindings for function" in {
      val cpg            = code("function method(x) {}")
      val List(typeDecl) = cpg.typeDecl.nameExact("method").l
      typeDecl.fullName should endWith("Test0.js::program:method")

      val List(binding) = typeDecl.bindsOut.cast[Binding].l
      binding.name shouldBe ""
      binding.signature shouldBe ""

      val List(boundMethod) = binding.refOut.l
      boundMethod shouldBe cpg.method.nameExact("method").head
    }

    "have correct structure for empty method" in {
      val cpg          = code("function method(x) {}")
      val List(method) = cpg.method.nameExact("method").l
      method.astChildren.isBlock.size shouldBe 1
      method.parameter.index(0).nameExact("this").typeFullName(Defines.Any).size shouldBe 1
      method.parameter.index(1).nameExact("x").typeFullName(Defines.Any).size shouldBe 1
    }

    "have correct structure for empty method with rest parameter" in {
      val cpg              = code("function method(x, ...args) {}")
      val List(method)     = cpg.method.nameExact("method").l
      val List(t, x, args) = method.parameter.l
      t.index shouldBe 0
      t.name shouldBe "this"
      t.typeFullName shouldBe Defines.Any
      t.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")
      x.index shouldBe 1
      x.name shouldBe "x"
      x.typeFullName shouldBe Defines.Any
      args.index shouldBe 2
      args.name shouldBe "args"
      args.code shouldBe "...args"
      args.isVariadic shouldBe true
      args.typeFullName shouldBe Defines.Any
    }

    "have correct structure for decl assignment" in {
      val cpg          = code("function foo(x) { var local = 1; }")
      val List(method) = cpg.method.nameExact("foo").l
      val List(block)  = method.astChildren.isBlock.l

      val List(t, x) = method.parameter.l
      t.index shouldBe 0
      t.name shouldBe "this"
      t.typeFullName shouldBe Defines.Any
      t.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")
      x.index shouldBe 1
      x.name shouldBe "x"
      x.typeFullName shouldBe Defines.Any

      val List(local) = block.astChildren.isLocal.l
      local.name shouldBe "local"

      val List(assignmentCall) = block.astChildren.isCall.l
      val List(assignmentOut)  = assignmentCall.astChildren.isIdentifier.l
      assignmentOut.name shouldBe "local"
    }

    "have correct structure for decl assignment with identifier on right hand side" in {
      val cpg          = code("function foo(x) { var local = x; }")
      val List(method) = cpg.method.nameExact("foo").l
      val List(block)  = method.astChildren.isBlock.l

      val List(t, x) = method.parameter.l
      t.index shouldBe 0
      t.name shouldBe "this"
      t.typeFullName shouldBe Defines.Any
      t.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")
      x.index shouldBe 1
      x.name shouldBe "x"
      x.typeFullName shouldBe Defines.Any

      val List(local) = block.astChildren.isLocal.l
      local.name shouldBe "local"

      val List(assignmentCall) = block.astChildren.isCall.l
      val List(localVar, xVar) = assignmentCall.astChildren.isIdentifier.l
      localVar.name shouldBe "local"
      xVar.name shouldBe "x"
    }

    "have correct structure for decl assignment of multiple locals" in {
      val cpg          = code("function foo(x,y) { var local1 = x; var local2 = y; }")
      val List(method) = cpg.method.nameExact("foo").l
      val List(block)  = method.astChildren.isBlock.l

      val List(t, x, y) = method.parameter.l
      t.index shouldBe 0
      t.name shouldBe "this"
      t.typeFullName shouldBe Defines.Any
      t.dynamicTypeHintFullName shouldBe Seq("Test0.js::program")
      x.index shouldBe 1
      x.name shouldBe "x"
      x.typeFullName shouldBe Defines.Any
      y.index shouldBe 2
      y.name shouldBe "y"
      y.typeFullName shouldBe Defines.Any

      val List(firstLocal, secondLocal) = block.astChildren.isLocal.l
      firstLocal.name shouldBe "local1"
      secondLocal.name shouldBe "local2"

      val List(firstAssigment, secondAssigment) = block.astChildren.isCall.l
      firstAssigment.code shouldBe "var local1 = x"
      secondAssigment.code shouldBe "var local2 = y"

      val List(outLocal1, outRight1) = firstAssigment.astChildren.isIdentifier.l
      outLocal1.name shouldBe "local1"
      outRight1.name shouldBe "x"

      val List(outLocal2, outRight2) = secondAssigment.astChildren.isIdentifier.l
      outLocal2.name shouldBe "local2"
      outRight2.name shouldBe "y"
    }

    "be correct for nested expression" in {
      val cpg                  = code("function method() { var x; var y; var z; x = y + z; }")
      val List(method)         = cpg.method.nameExact("method").l
      val List(block)          = method.astChildren.isBlock.l
      val List(assignmentCall) = block.astChildren.isCall.l
      val List(identifierX)    = assignmentCall.astChildren.isIdentifier.l
      identifierX.name shouldBe "x"

      val List(plus)                     = assignmentCall.astChildren.isCall.l
      val List(identifierY, identifierZ) = plus.astChildren.isIdentifier.l
      identifierY.name shouldBe "y"
      identifierZ.name shouldBe "z"
    }

    "be correct for while loop" in {
      val cpg = code("""
          |function method(x) {
          |  while (x < 1) {
          |    x += 1;
          |  }
          |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(whileNode) = methodBlock.astChildren.isControlStructure.l
      whileNode.controlStructureType shouldBe ControlStructureTypes.WHILE
      whileNode.order shouldBe 1

      val List(whileCondition) = whileNode.astChildren.isCall.l
      whileCondition.code shouldBe "x < 1"
      whileCondition.order shouldBe 1

      val List(whileBlock) = whileNode.astChildren.isBlock.l
      whileBlock.order shouldBe 2

      val List(assign) = whileBlock.astChildren.isCall.l
      assign.code shouldBe "x += 1"
      assign.order shouldBe 1

      val List(identifierX) = assign.astChildren.isIdentifier.l
      identifierX.code shouldBe "x"
      identifierX.order shouldBe 1

      val List(literal1) = assign.astChildren.isLiteral.l
      literal1.code shouldBe "1"
      literal1.order shouldBe 2
    }

    "be correct for if" in {
      val cpg = code("""
          |function method(x) {
          |  var y;
          |  if (x > 0)
          |    y = 0;
          |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      methodBlock.astChildren.isLocal.size shouldBe 1

      val List(ifNode) = methodBlock.astChildren.isControlStructure.l
      ifNode.controlStructureType shouldBe ControlStructureTypes.IF
      ifNode.order shouldBe 1

      val List(ifCondition) = ifNode.astChildren.isCall.order(1).l
      ifCondition.code shouldBe "x > 0"

      val List(assignment) = ifNode.astChildren.isCall.order(2).l
      assignment.code shouldBe "y = 0"

      val List(identifierY) = assignment.astChildren.isIdentifier.l
      identifierY.code shouldBe "y"
      identifierY.order shouldBe 1

      val List(literal0) = assignment.astChildren.isLiteral.l
      literal0.code shouldBe "0"
      literal0.order shouldBe 2
    }

    "be correct for if-else" in {
      val cpg = code("""
         |function method(x) {
         |  var y;
         |  if (x > 0) {
         |    y = 0;
         |  } else {
         |    y = 1;
         |  }
         |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(localY)      = methodBlock.astChildren.isLocal.l
      localY.order shouldBe 0

      val List(ifNode) = methodBlock.astChildren.isControlStructure.l
      ifNode.controlStructureType shouldBe ControlStructureTypes.IF
      ifNode.order shouldBe 1

      val List(ifCondition) = ifNode.astChildren.isCall.l
      ifCondition.code shouldBe "x > 0"
      ifCondition.order shouldBe 1

      val List(ifBlock)       = ifNode.astChildren.isBlock.order(2).l
      val List(ifBlockAssign) = ifBlock.astChildren.isCall.l
      ifBlockAssign.code shouldBe "y = 0"
      ifBlockAssign.order shouldBe 1

      val List(elseBlock)       = ifNode.astChildren.isBlock.order(3).l
      val List(elseBlockAssign) = elseBlock.astChildren.isCall.l
      elseBlockAssign.code shouldBe "y = 1"
      elseBlockAssign.order shouldBe 1
    }

    "be correct for for-loop with for-of with object destruction" in {
      val cpg = code("""
        |for(var {a, b, c} of obj) {
        |   foo(a, b, c)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOfObject(loopBlock)
    }

    "be correct for for-loop with for-of with object destruction without declaration" in {
      val cpg = code("""
        |for({a, b, c} of obj) {
        |   foo(a, b, c)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOfObject(loopBlock)
    }

    "be correct for for-loop with for-of with array destruction" in {
      val cpg = code("""
        |for(var [a, b, c] of arr) {
        |   foo(a, b, c)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOfArray(loopBlock)
    }

    "be correct for for-loop with for-of with array destruction without declaration" in {
      val cpg = code("""
        |for([a, b, c] of arr) {
        |   foo(a, b, c)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOfArray(loopBlock)
    }

    "be correct for for-loop with for-in" in {
      val cpg = code("""
        |for (var i in arr) {
        |   foo(i)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "be correct for for-loop with for-in  without declaration" in {
      val cpg = code("""
        |for (i in arr) {
        |   foo(i)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "be correct for for-loop with for-of" in {
      val cpg = code("""
        |for (var i of arr) {
        |   foo(i)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "be correct for for-loop with for-of without declaration" in {
      val cpg = code("""
        |for (i of arr) {
        |   foo(i)
        |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForInOrOf(loopBlock)
    }

    "be correct for for-loop with empty test" in {
      val cpg               = code("for(;;){}")
      val List(method)      = cpg.method.nameExact(":program").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(forNode)     = methodBlock.astChildren.isControlStructure.l
      forNode.controlStructureType shouldBe ControlStructureTypes.FOR
      forNode.order shouldBe 1

      val List(forCondition) = forNode.astChildren.isLiteral.order(2).l
      forCondition.code shouldBe "true"
    }

    "be correct for for-loop" in {
      val cpg = code("""
        |function method(x,y) {
        |  for (x = 0; x < 1; x += 1) {
        |    z = 0;
        |  }
        |}
        """.stripMargin)
      val List(method) = cpg.method.nameExact("method").l

      val List(parameterInX) = method.parameter.order(1).l
      parameterInX.name shouldBe "x"

      val List(parameterInY) = method.parameter.order(2).l
      parameterInY.name shouldBe "y"

      val List(methodBlock) = method.astChildren.isBlock.l

      val List(forNode) = methodBlock.astChildren.isControlStructure.l
      forNode.controlStructureType shouldBe ControlStructureTypes.FOR
      forNode.order shouldBe 1

      val List(forInit) = forNode.astChildren.isCall.order(1).l
      forInit.code shouldBe "x = 0"

      val List(forCondition) = forNode.astChildren.isCall.order(2).l
      forCondition.code shouldBe "x < 1"

      val List(forModify) = forNode.astChildren.isCall.order(3).l
      forModify.code shouldBe "x += 1"

      val List(forBlock) = forNode.astChildren.isBlock.l
      forBlock.order shouldBe 4

      val List(forBlockAssign) = forBlock.astChildren.isCall.l
      forBlockAssign.code shouldBe "z = 0"
      forBlockAssign.order shouldBe 1
    }

    "handle labeled statements and" should {

      "be correct for continue" in {
        val cpg = code("""
        |var i, j;
        |loop1: for (i = 0; i < 3; i++) {
        |   loop2: for (j = 0; j < 3; j++) {
        |      if (i === 1 && j === 1) {
        |         continue loop1;
        |      }
        |      console.log("i = " + i + ", j = " + j);
        |   }
        |}
        |""".stripMargin)
        inside(cpg.jumpTarget.l) { case List(loop1, loop2) =>
          loop1.code shouldBe "loop1:"
          loop2.code shouldBe "loop2:"
        }
        inside(cpg.controlStructure.code("continue.*").l) { case List(continue) =>
          continue.code shouldBe "continue loop1;"
          continue.controlStructureType shouldBe ControlStructureTypes.CONTINUE
        }
      }

    }

    "handle switch statements and" should {
      "be correct for switch with one case" in {
        val cpg                = code("switch (x) { case 1: y; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel)   = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
        caseLabel.order shouldBe 1
        val List(caseExpr) = switchBlock.astChildren.isLiteral.codeExact("1").l
        caseExpr.order shouldBe 2
        val List(identifierY) = switchBlock.astChildren.isIdentifier.codeExact("y").l
        identifierY.order shouldBe 3
      }

      "be correct for switch with multiple cases" in {
        val cpg                = code("switch (x) { case 1: y; case 2: z; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH
        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
        caseLabel1.order shouldBe 1

        val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("1").l
        caseExpr1.order shouldBe 2

        val List(identifierY) = switchBlock.astChildren.isIdentifier.codeExact("y").l
        identifierY.order shouldBe 3

        val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("case 2:").l
        caseLabel2.order shouldBe 4

        val List(caseExpr2) = switchBlock.astChildren.isLiteral.codeExact("2").l
        caseExpr2.order shouldBe 5

        val List(identifierZ) = switchBlock.astChildren.isIdentifier.codeExact("z").l
        identifierZ.order shouldBe 6
      }

      "be correct for switch with multiple cases on same spot" in {
        val cpg                = code("switch (x) { case 1: case 2: y; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
        caseLabel1.order shouldBe 1

        val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("1").l
        caseExpr1.order shouldBe 2

        val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("case 2:").l
        caseLabel2.order shouldBe 3

        val List(caseExpr2) = switchBlock.astChildren.isLiteral.codeExact("2").l
        caseExpr2.order shouldBe 4

        val List(identifierY) = switchBlock.astChildren.isIdentifier.codeExact("y").l
        identifierY.order shouldBe 5
      }

      "be correct for switch with multiple cases and multiple cases on same spot" in {
        val cpg                = code("switch (x) { case 1: case 2: y; case 3: z; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
        caseLabel1.order shouldBe 1

        val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("1").l
        caseExpr1.order shouldBe 2

        val List(caseLabel2) =
          switchBlock._jumpTargetViaAstOut.codeExact("case 2:").l
        caseLabel2.order shouldBe 3

        val List(caseExpr2) = switchBlock.astChildren.isLiteral.codeExact("2").l
        caseExpr2.order shouldBe 4

        val List(identifierY) =
          switchBlock.astChildren.isIdentifier.codeExact("y").l
        identifierY.order shouldBe 5

        val List(caseLabel3) =
          switchBlock._jumpTargetViaAstOut.codeExact("case 3:").l
        caseLabel3.order shouldBe 6

        val List(caseExpr3) = switchBlock.astChildren.isLiteral.codeExact("3").l
        caseExpr3.order shouldBe 7

        val List(identifierZ) =
          switchBlock.astChildren.isIdentifier.codeExact("z").l
        identifierZ.order shouldBe 8
      }

      "be correct for switch with default case" in {
        val cpg                = code("switch (x) { default: y; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        programBlock.astChildren.isLiteral.size shouldBe 0

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel)   = switchBlock._jumpTargetViaAstOut.codeExact("default:").l
        caseLabel.order shouldBe 1

        val List(identifierY) = switchBlock.astChildren.isIdentifier.nameExact("y").l
        identifierY.order shouldBe 2
      }

      "be correct for switch with case and default combined" in {
        val cpg                = code("switch (x) { case 1: y; break; default: z; }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isIdentifier.nameExact("x").l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "x"

        val List(switchBlock) = switch.astChildren.isBlock.l
        val List(caseLabel1)  = switchBlock._jumpTargetViaAstOut.codeExact("case 1:").l
        caseLabel1.order shouldBe 1

        val List(caseExpr1) = switchBlock.astChildren.isLiteral.codeExact("1").l
        caseExpr1.order shouldBe 2

        val List(identifierY) = switchBlock.astChildren.isIdentifier.nameExact("y").l
        identifierY.order shouldBe 3

        val List(break) =
          switchBlock.astChildren.isControlStructure.controlStructureTypeExact(ControlStructureTypes.BREAK).l
        break.order shouldBe 4

        val List(caseLabel2) = switchBlock._jumpTargetViaAstOut.codeExact("default:").l
        caseLabel2.order shouldBe 5

        val List(identifierZ) = switchBlock.astChildren.isIdentifier.nameExact("z").l
        identifierZ.order shouldBe 6
      }

      "be correct for switch with nested switch" in {
        val cpg                = code("switch (x) { default: switch(y) { default: z; } }")
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(topLevelSwitch) = programBlock.astChildren.isControlStructure.l
        topLevelSwitch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(topLevelSwitchExpr) = topLevelSwitch.astChildren.isIdentifier.nameExact("x").l
        topLevelSwitchExpr.order shouldBe 1
        topLevelSwitchExpr.code shouldBe "x"

        val List(topLevelSwitchBlock) = topLevelSwitch.astChildren.isBlock.l

        val List(topLevelCaseLabel) = topLevelSwitchBlock._jumpTargetViaAstOut.codeExact("default:").l
        topLevelCaseLabel.order shouldBe 1

        val List(nestedSwitch) = topLevelSwitchBlock.astChildren.isControlStructure.l
        nestedSwitch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(nestedSwitchExpr) = nestedSwitch.astChildren.isIdentifier.nameExact("y").l
        nestedSwitchExpr.order shouldBe 1
        nestedSwitchExpr.code shouldBe "y"

        val List(nestedSwitchBlock) = nestedSwitch.astChildren.isBlock.l
        val List(nestedCaseLabel)   = nestedSwitchBlock._jumpTargetViaAstOut.codeExact("default:").l
        nestedCaseLabel.order shouldBe 1

        val List(identifierZ) = nestedSwitchBlock.astChildren.isIdentifier.nameExact("z").l
        identifierZ.order shouldBe 2
      }

      "be correct for switch with lambda" in {
        val cpg = code("""
          |switch ((x) => "") { }
          |""".stripMargin)
        val List(program)      = cpg.method.nameExact(":program").l
        val List(programBlock) = program.astChildren.isBlock.l

        val List(switch) = programBlock.astChildren.isControlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH

        val List(switchExpr) = switch.astChildren.isMethodRef.l
        switchExpr.order shouldBe 1
        switchExpr.code shouldBe "<lambda>0"
      }
    }

    "be correct for logical expression '++'" in {
      val cpg = code("""
         |function method(x) {
         |  true && false;
         |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(logicalCall) = method.astChildren.isBlock.astChildren.isCall.l
      logicalCall.name shouldBe Operators.logicalAnd
      val List(callArg1) = logicalCall.astChildren.isLiteral.argumentIndex(1).l
      callArg1.code shouldBe "true"

      val List(callArg2) = logicalCall.astChildren.isLiteral.argumentIndex(2).l
      callArg2.code shouldBe "false"
    }

    "be correct for unary expression '++'" in {
      val cpg = code("""
         |function method(x) {
         |  ++x;
         |}
        """.stripMargin)
      val List(method)        = cpg.method.nameExact("method").l
      val List(methodBlock)   = method.astChildren.isBlock.l
      val List(unaryPlusCall) = methodBlock.astChildren.isCall.l
      unaryPlusCall.code shouldBe "++x"
      val List(identifierX) = unaryPlusCall.astChildren.isIdentifier.l
      identifierX.name shouldBe "x"
    }

    "be correct for member access used in an assignment (direct)" in {
      val cpg = code("""
          |function method(x) {
          |  z = x.a;
          |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      assignment.methodFullName shouldBe Operators.assignment

      val List(identifierZ) = assignment.astChildren.isIdentifier.l
      identifierZ.name shouldBe "z"

      val List(rightHandSide) = assignment.astChildren.isCall.l
      rightHandSide.methodFullName shouldBe Operators.fieldAccess

      val List(identifierRightX) = rightHandSide.astChildren.isIdentifier.argumentIndex(1).l
      identifierRightX.name shouldBe "x"
      identifierRightX.code shouldBe "x"

      val List(identifierRightA) = rightHandSide.astChildren.isFieldIdentifier.argumentIndex(2).l
      identifierRightA.canonicalName shouldBe "a"
      identifierRightA.code shouldBe "a"
    }

    "be correct for member access used in an assignment (chained)" in {
      val cpg = code("""
         |function method(x) {
         |  z = x.a.b.c;
         |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      assignment.methodFullName shouldBe Operators.assignment

      val List(identifierZ) = assignment.astChildren.isIdentifier.l
      identifierZ.name shouldBe "z"

      val List(rightC) = assignment.astChildren.isCall.l
      rightC.methodFullName shouldBe Operators.fieldAccess

      val List(identifierC) = rightC.astChildren.isFieldIdentifier.l
      identifierC.canonicalName shouldBe "c"

      val List(rightB) = rightC.astChildren.isCall.l
      rightB.methodFullName shouldBe Operators.fieldAccess

      val List(identifierB) = rightB.astChildren.isFieldIdentifier.l
      identifierB.canonicalName shouldBe "b"

      val List(rightA) = rightB.astChildren.isCall.l
      rightA.methodFullName shouldBe Operators.fieldAccess

      val List(identifierX) = rightA.astChildren.isIdentifier.argumentIndex(1).l
      identifierX.name shouldBe "x"

      val List(identifierA) = rightA.astChildren.isFieldIdentifier.argumentIndex(2).l
      identifierA.canonicalName shouldBe "a"
    }

    "be correct for member access used in an assignment (chained with method call)" in {
      val cpg = code("""
          |function method(x) {
          |  z = x.a.b.c();
          |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      assignment.methodFullName shouldBe Operators.assignment

      val List(identifierZ) = assignment.astChildren.isIdentifier.l
      identifierZ.name shouldBe "z"

      val List(right) = assignment.astChildren.isCall.l
      right.name shouldBe "c"

      val List(callToC) = right.astChildren.isCall.l
      callToC.methodFullName shouldBe Operators.fieldAccess

      val List(identifierC) = callToC.astChildren.isFieldIdentifier.l
      identifierC.canonicalName shouldBe "c"

      val List(assignmentToTmp) = callToC.astChildren.isCall.l
      assignmentToTmp.methodFullName shouldBe Operators.assignment

      val List(tmpIdentifier) = assignmentToTmp.astChildren.isIdentifier.l
      tmpIdentifier.name shouldBe "_tmp_0"

      val List(fieldAccessXAB) = assignmentToTmp.astChildren.isCall.l
      fieldAccessXAB.methodFullName shouldBe Operators.fieldAccess

      val List(identifierB) = fieldAccessXAB.astChildren.isFieldIdentifier.l
      identifierB.canonicalName shouldBe "b"

      val List(callToA) = fieldAccessXAB.astChildren.isCall.l
      callToA.methodFullName shouldBe Operators.fieldAccess

      val List(identifierX) = callToA.astChildren.isIdentifier.argumentIndex(1).l
      identifierX.name shouldBe "x"

      val List(identifierA) = callToA.astChildren.isFieldIdentifier.argumentIndex(2).l
      identifierA.canonicalName shouldBe "a"
    }

    "be correct for member access used as return" in {
      val cpg = code("""
      |function method(x) {
      |  return x.a;
      |}""".stripMargin)
      val List(method)          = cpg.method.nameExact("method").l
      val List(methodBlock)     = method.astChildren.isBlock.l
      val List(returnStatement) = methodBlock.astChildren.isReturn.l
      val List(rightHandSide)   = returnStatement.astChildren.isCall.l
      rightHandSide.order shouldBe 1
      rightHandSide.argumentIndex shouldBe 1
      rightHandSide.methodFullName shouldBe Operators.fieldAccess

      val List(identifierX) = rightHandSide.astChildren.isIdentifier.argumentIndex(1).l
      identifierX.name shouldBe "x"

      val List(identifierA) = rightHandSide.astChildren.isFieldIdentifier.argumentIndex(2).l
      identifierA.canonicalName shouldBe "a"
    }

    "be correct for function used as return" in {
      val cpg = code("""
        |function method(x) {
        |  return function foo() {};
        |}
        """.stripMargin)
      val List(method) = cpg.method.nameExact("method").l
      val List(ref)    = method.ast.isReturn.astChildren.isMethodRef.l
      ref.code shouldBe "foo"
    }

    "be correct for member access as useless statement" in {
      val cpg = code("""
          |function method(x) {
          |  x.a;
          |}
        """.stripMargin)
      val List(method)      = cpg.method.nameExact("method").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(statement)   = methodBlock.astChildren.isCall.l
      statement.methodFullName shouldBe Operators.fieldAccess

      val List(identifierX) = statement.astChildren.isIdentifier.argumentIndex(1).l
      identifierX.name shouldBe "x"

      val List(identifierA) = statement.astChildren.isFieldIdentifier.argumentIndex(2).l
      identifierA.canonicalName shouldBe "a"
    }

    "be correct for empty method" in {
      val cpg           = code("function method() {}")
      val List(program) = cpg.method.nameExact("method").l
      program.astChildren.isBlock.size shouldBe 1
      val blockMethodReturn = program.methodReturn
      blockMethodReturn.code shouldBe "RET"
    }

  }

  private def checkObjectInitialization(node: Block, member: (String, String)): Unit = {
    val (keyName, assignedValue) = member

    val List(localTmp) = node.astChildren.isLocal.nameExact("_tmp_0").l
    localTmp.order shouldBe 0

    val List(tmp) = node.astChildren.isIdentifier.nameExact("_tmp_0").l
    tmp.code shouldBe "_tmp_0"

    val List(call) = node.astChildren.isCall.codeExact(s"_tmp_0.$keyName = $assignedValue").l
    call.methodFullName shouldBe Operators.assignment

    val List(tmpAccess) = call.argument(1).start.isCall.l
    tmpAccess.code shouldBe s"_tmp_0.$keyName"
    tmpAccess.methodFullName shouldBe Operators.fieldAccess
    tmpAccess.argumentIndex shouldBe 1
    val List(value) = call.argument(2).start.l
    value.code shouldBe assignedValue

    val List(leftHandSideTmpId) = tmpAccess.astChildren.isIdentifier.nameExact("_tmp_0").l
    leftHandSideTmpId.code shouldBe "_tmp_0"

    val List(key) = tmpAccess.astChildren.isFieldIdentifier.l
    key.canonicalName shouldBe keyName
  }

  private def checkForInOrOfObject(node: Block): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(localA) = node.astChildren.isLocal.nameExact("a").l
    localA.code shouldBe "a"
    val List(localB) = node.astChildren.isLocal.nameExact("b").l
    localB.code shouldBe "b"
    val List(localC) = node.astChildren.isLocal.nameExact("c").l
    localC.code shouldBe "c"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(obj)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(obj)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "obj"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(varA) = node.astChildren.isIdentifier.nameExact("a").l
    varA.code shouldBe "a"
    val List(varB) = node.astChildren.isIdentifier.nameExact("b").l
    varB.code shouldBe "b"
    val List(varC) = node.astChildren.isIdentifier.nameExact("c").l
    varC.code shouldBe "c"

    val List(loop) = node.astChildren.isControlStructure.l
    loop.controlStructureType shouldBe ControlStructureTypes.WHILE

    val List(loopTestCall) = loop.astChildren.isCall.codeExact("!(_result_0 = _iterator_0.next()).done").l
    loopTestCall.name shouldBe Operators.not
    loopTestCall.order shouldBe 1

    val List(doneMaCall) = loopTestCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next()).done").l
    doneMaCall.name shouldBe Operators.fieldAccess

    val List(doneMaBase) = doneMaCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next())").l
    doneMaBase.name shouldBe Operators.assignment
    doneMaBase.order shouldBe 1
    doneMaBase.argumentIndex shouldBe 1

    val List(doneMaBaseLhs) = doneMaBase.astChildren.isIdentifier.order(1).l
    doneMaBaseLhs.name shouldBe "_result_0"
    doneMaBaseLhs.argumentIndex shouldBe 1

    val List(doneMaBaseRhs) = doneMaBase.astChildren.isCall.order(2).l
    doneMaBaseRhs.code shouldBe "_iterator_0.next()"
    doneMaBaseRhs.argumentIndex shouldBe 2

    val List(doneMember) = doneMaCall.astChildren.isFieldIdentifier.canonicalNameExact("done").l
    doneMember.order shouldBe 2
    doneMember.argumentIndex shouldBe 2

    val List(whileLoopBlock) = loop.astChildren.isBlock.l
    whileLoopBlock.order shouldBe 2

    val List(loopVarAssignmentCallA) = whileLoopBlock.astChildren.isCall.codeExact("a = _result_0.value.a").l
    loopVarAssignmentCallA.name shouldBe Operators.assignment
    loopVarAssignmentCallA.order shouldBe 1
    val List(loopVarAssignmentCallB) = whileLoopBlock.astChildren.isCall.codeExact("b = _result_0.value.b").l
    loopVarAssignmentCallB.name shouldBe Operators.assignment
    loopVarAssignmentCallB.order shouldBe 2
    val List(loopVarAssignmentCallC) = whileLoopBlock.astChildren.isCall.codeExact("c = _result_0.value.c").l
    loopVarAssignmentCallC.name shouldBe Operators.assignment
    loopVarAssignmentCallC.order shouldBe 3

    val List(fooCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("foo(a, b, c)").l
    fooCall.name shouldBe "foo"
  }

  private def checkForInOrOfArray(node: Block): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(localA) = node.astChildren.isLocal.nameExact("a").l
    localA.code shouldBe "a"
    val List(localB) = node.astChildren.isLocal.nameExact("b").l
    localB.code shouldBe "b"
    val List(localC) = node.astChildren.isLocal.nameExact("c").l
    localC.code shouldBe "c"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(arr)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(arr)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "arr"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(varA) = node.astChildren.isIdentifier.nameExact("a").l
    varA.code shouldBe "a"
    val List(varB) = node.astChildren.isIdentifier.nameExact("b").l
    varB.code shouldBe "b"
    val List(varC) = node.astChildren.isIdentifier.nameExact("c").l
    varC.code shouldBe "c"

    val List(loop) = node.astChildren.isControlStructure.l
    loop.controlStructureType shouldBe ControlStructureTypes.WHILE

    val List(loopTestCall) = loop.astChildren.isCall.codeExact("!(_result_0 = _iterator_0.next()).done").l
    loopTestCall.name shouldBe Operators.not
    loopTestCall.order shouldBe 1

    val List(doneMaCall) = loopTestCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next()).done").l
    doneMaCall.name shouldBe Operators.fieldAccess

    val List(doneMaBase) = doneMaCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next())").l
    doneMaBase.name shouldBe Operators.assignment
    doneMaBase.order shouldBe 1
    doneMaBase.argumentIndex shouldBe 1

    val List(doneMaBaseLhs) = doneMaBase.astChildren.isIdentifier.order(1).l
    doneMaBaseLhs.name shouldBe "_result_0"
    doneMaBaseLhs.argumentIndex shouldBe 1

    val List(doneMaBaseRhs) = doneMaBase.astChildren.isCall.order(2).l
    doneMaBaseRhs.code shouldBe "_iterator_0.next()"
    doneMaBaseRhs.argumentIndex shouldBe 2

    val List(doneMember) = doneMaCall.astChildren.isFieldIdentifier.canonicalNameExact("done").l
    doneMember.order shouldBe 2
    doneMember.argumentIndex shouldBe 2

    val List(whileLoopBlock) = loop.astChildren.isBlock.l
    whileLoopBlock.order shouldBe 2

    val List(loopVarAssignmentCallA) = whileLoopBlock.astChildren.isCall.codeExact("a = _result_0.value[0]").l
    loopVarAssignmentCallA.name shouldBe Operators.assignment
    loopVarAssignmentCallA.order shouldBe 1
    val List(loopVarAssignmentCallB) = whileLoopBlock.astChildren.isCall.codeExact("b = _result_0.value[1]").l
    loopVarAssignmentCallB.name shouldBe Operators.assignment
    loopVarAssignmentCallB.order shouldBe 2
    val List(loopVarAssignmentCallC) = whileLoopBlock.astChildren.isCall.codeExact("c = _result_0.value[2]").l
    loopVarAssignmentCallC.name shouldBe Operators.assignment
    loopVarAssignmentCallC.order shouldBe 3

    val List(fooCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("foo(a, b, c)").l
    fooCall.name shouldBe "foo"
  }

  private def checkForInOrOf(node: Block): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(localI) = node.astChildren.isLocal.nameExact("i").l
    localI.code shouldBe "i"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(arr)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(arr)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "arr"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(varI) = node.astChildren.isIdentifier.nameExact("i").l
    varI.code shouldBe "i"

    val List(loop) = node.astChildren.isControlStructure.l
    loop.controlStructureType shouldBe ControlStructureTypes.WHILE

    val List(loopTestCall) = loop.astChildren.isCall.codeExact("!(_result_0 = _iterator_0.next()).done").l
    loopTestCall.name shouldBe Operators.not
    loopTestCall.order shouldBe 1

    val List(doneMaCall) = loopTestCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next()).done").l
    doneMaCall.name shouldBe Operators.fieldAccess

    val List(doneMaBase) = doneMaCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next())").l
    doneMaBase.name shouldBe Operators.assignment
    doneMaBase.order shouldBe 1
    doneMaBase.argumentIndex shouldBe 1

    val List(doneMaBaseLhs) = doneMaBase.astChildren.isIdentifier.order(1).l
    doneMaBaseLhs.name shouldBe "_result_0"
    doneMaBaseLhs.argumentIndex shouldBe 1

    val List(doneMaBaseRhs) = doneMaBase.astChildren.isCall.order(2).l
    doneMaBaseRhs.code shouldBe "_iterator_0.next()"
    doneMaBaseRhs.argumentIndex shouldBe 2

    val List(doneMember) = doneMaCall.astChildren.isFieldIdentifier.canonicalNameExact("done").l
    doneMember.order shouldBe 2
    doneMember.argumentIndex shouldBe 2

    val List(whileLoopBlock) = loop.astChildren.isBlock.l
    whileLoopBlock.order shouldBe 2

    val List(loopVarAssignmentCall) = whileLoopBlock.astChildren.isCall.codeExact("i = _result_0.value").l
    loopVarAssignmentCall.name shouldBe Operators.assignment
    loopVarAssignmentCall.order shouldBe 1

    val List(fooCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("foo(i)").l
    fooCall.name shouldBe "foo"
  }

  private def checkLiterals(node: Block, element: Int): Unit = {
    val List(pushCall) = node.astChildren.isCall.codeExact(s"_tmp_0.push($element)").l

    val List(pushCallReceiver) = pushCall.receiver.isCall.l
    pushCallReceiver.name shouldBe Operators.fieldAccess
    pushCallReceiver.argumentIndex shouldBe -1

    val pushCallReceiverBase = pushCallReceiver.argument(1).asInstanceOf[Identifier]
    pushCallReceiverBase.name shouldBe "_tmp_0"

    val pushCallReceiverMember = pushCallReceiver.argument(2).asInstanceOf[FieldIdentifier]
    pushCallReceiverMember.canonicalName shouldBe "push"

    val pushCallThis = pushCall.argument(0).asInstanceOf[Identifier]
    pushCallThis.name shouldBe "_tmp_0"

    val pushCallArg = pushCall.argument(1).asInstanceOf[Literal]
    pushCallArg.code shouldBe element.toString
  }

}
