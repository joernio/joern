package io.joern.jssrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  NodeTypes,
  Operators,
  PropertyNames
}
import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.codepropertygraph.generated.nodes.NamespaceBlock
import io.shiftleft.semanticcpg.language._
import overflowdb.Node
import overflowdb.traversal.Traversal

class AstCreationPassTest extends AbstractPassTest {

  "AST generation for simple fragments" should {

    "have correct structure for FILENAME property" in AstFixture("let x = 1;") { cpg =>
      def namespaceBlocks = cpg.namespaceBlock.filter(PropertyNames.FILENAME, "code.js")
      namespaceBlocks.checkNodeCount(1)

      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)
      program.checkProperty(PropertyNames.FILENAME, "code.js")

      def typeDecls = cpg.typeDecl.nameExact(":program")
      typeDecls.checkNodeCount(1)
      typeDecls.checkProperty(PropertyNames.FILENAME, "code.js")
    }

    "have correct structure for block expression" in AstFixture("let x = (class Foo {}, bar())") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def classFooMetaTypeDecl =
        cpg.typeDecl
          .nameExact("Foo<meta>")
          .filter(PropertyNames.FULL_NAME, "code.js::program:Foo<meta>")
      classFooMetaTypeDecl.checkNodeCount(1)

      def classFooTypeDecl =
        cpg.typeDecl.nameExact("Foo").filter(PropertyNames.FULL_NAME, "code.js::program:Foo")
      classFooTypeDecl.checkNodeCount(1)

      // constructor
      def classFooMethod =
        classFooTypeDecl
          .expandAst(NodeTypes.METHOD)
          .filter(PropertyNames.NAME, "Foo<constructor>")
      classFooMethod.checkNodeCount(1)
      classFooMethod.checkProperty(PropertyNames.CODE, "constructor() {}")

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def assignment = programBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)
      assignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def commaRight = assignment.expandAst(NodeTypes.BLOCK)
      commaRight.checkNodeCount(1)
      commaRight.expandAst().checkNodeCount(2)

      def refForConstructor = commaRight.expandAst(NodeTypes.TYPE_REF)
      refForConstructor.checkNodeCount(1)
      refForConstructor.checkProperty(PropertyNames.CODE, "class Foo")

      def barCall = commaRight.expandAst(NodeTypes.CALL)
      barCall.checkNodeCount(1)
      barCall.checkProperty(PropertyNames.CODE, "bar()")
    }

    "have correct structure for empty array literal" in AstFixture("var x = []") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def xAssignment = methodBlock.expandAst(NodeTypes.CALL)
      xAssignment.checkNodeCount(1)
      xAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def arrayCall = xAssignment.expandAst(NodeTypes.CALL)
      arrayCall.checkNodeCount(1)
      arrayCall.checkProperty(PropertyNames.NAME, EcmaBuiltins.arrayFactory)
      arrayCall.checkProperty(PropertyNames.CODE, EcmaBuiltins.arrayFactory + "()")
      arrayCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)
    }

    "have correct structure for array literal with values" in AstFixture("var x = [1, 2]") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def xAssignment = methodBlock.expandAst(NodeTypes.CALL)
      xAssignment.checkNodeCount(1)
      xAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def pushBlock = xAssignment.expandAst(NodeTypes.BLOCK)
      pushBlock.checkNodeCount(1)

      def tmpLocal = pushBlock.expandAst(NodeTypes.LOCAL)
      tmpLocal.checkNodeCount(1)
      tmpLocal.checkProperty(PropertyNames.NAME, "_tmp_0")

      def tmpAssignment =
        pushBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_0 = __ecma.Array.factory()")
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def arrayCall = tmpAssignment.expandAst(NodeTypes.CALL)
      arrayCall.checkNodeCount(1)
      arrayCall.checkProperty(PropertyNames.NAME, EcmaBuiltins.arrayFactory)
      arrayCall.checkProperty(PropertyNames.CODE, EcmaBuiltins.arrayFactory + "()")
      arrayCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)

      checkLiterals(pushBlock.head, 1)
      checkLiterals(pushBlock.head, 2)

      def tmpReturn = pushBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturn.checkNodeCount(1)
      tmpReturn.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for untagged runtime node in call" in AstFixture(s"foo(`Hello $${world}!`)") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def fooCall = methodBlock.expandAst(NodeTypes.CALL)
      fooCall.checkNodeCount(1)
      fooCall.checkProperty(PropertyNames.CODE, """foo(__Runtime.TO_STRING("Hello ", world, "!"))""")

      def templateCall = fooCall.expandAst(NodeTypes.CALL)
      templateCall.checkNodeCount(1)
      templateCall.checkProperty(PropertyNames.NAME, "__Runtime.TO_STRING")
      templateCall.checkProperty(PropertyNames.CODE, """__Runtime.TO_STRING("Hello ", world, "!")""")

      def argument1 = templateCall.expandAst().filter(PropertyNames.ORDER, 1)
      argument1.checkNodeCount(1)
      argument1.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
      argument1.checkProperty(PropertyNames.CODE, "\"Hello \"")

      def argument2 = templateCall.expandAst().filter(PropertyNames.ORDER, 2)
      argument2.checkNodeCount(1)
      argument2.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)
      argument2.checkProperty(PropertyNames.CODE, "world")

      def argument3 = templateCall.expandAst().filter(PropertyNames.ORDER, 3)
      argument3.checkNodeCount(1)
      argument3.checkProperty(PropertyNames.ARGUMENT_INDEX, 3)
      argument3.checkProperty(PropertyNames.CODE, "\"!\"")
    }

    "have correct structure for untagged runtime node" in AstFixture(s"`$${x + 1}`") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def call = methodBlock.expandAst(NodeTypes.CALL)
      call.checkNodeCount(1)
      call.checkProperty(PropertyNames.NAME, "__Runtime.TO_STRING")
      call.checkProperty(PropertyNames.CODE, "__Runtime.TO_STRING(\"\", x + 1, \"\")")

      def argument1 = call.expandAst().filter(PropertyNames.ORDER, 1)
      argument1.checkNodeCount(1)
      argument1.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
      argument1.checkProperty(PropertyNames.CODE, "\"\"")

      def argument2 = call.expandAst().filter(PropertyNames.ORDER, 2)
      argument2.checkNodeCount(1)
      argument2.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)
      argument2.checkProperty(PropertyNames.CODE, "x + 1")

      def argument3 = call.expandAst().filter(PropertyNames.ORDER, 3)
      argument3.checkNodeCount(1)
      argument3.checkProperty(PropertyNames.ARGUMENT_INDEX, 3)
      argument3.checkProperty(PropertyNames.CODE, "\"\"")
    }

    "have correct structure for tagged runtime node" in AstFixture(s"String.raw`../$${42}\\..`") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def rawCall = methodBlock.expandAst(NodeTypes.CALL)
      rawCall.checkNodeCount(1)
      rawCall.checkProperty(PropertyNames.CODE, """String.raw(__Runtime.TO_STRING("../", 42, "\.."))""")

      def runtimeCall =
        rawCall.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "__Runtime.TO_STRING")
      runtimeCall.checkNodeCount(1)
      runtimeCall.checkProperty(PropertyNames.ORDER, 1)
      runtimeCall.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
      runtimeCall.checkProperty(PropertyNames.CODE, """__Runtime.TO_STRING("../", 42, "\..")""")

      def argument1 =
        runtimeCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "\"../\"")
      argument1.checkNodeCount(1)
      argument1.checkProperty(PropertyNames.ORDER, 1)
      argument1.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def argument2 = runtimeCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "42")
      argument2.checkNodeCount(1)
      argument2.checkProperty(PropertyNames.ORDER, 2)
      argument2.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

      def argument3 =
        runtimeCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "\"\\..\"")
      argument3.checkNodeCount(1)
      argument3.checkProperty(PropertyNames.ORDER, 3)
      argument3.checkProperty(PropertyNames.ARGUMENT_INDEX, 3)
    }

    "have correct structure for try" in AstFixture("""
       |try {
       | open()
       |} catch(err) {
       | handle()
       |} finally {
       | close()
       |}
       |""".stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def tryStatement = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      tryStatement.checkNodeCount(1)
      tryStatement.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.TRY)

      def blocks = tryStatement.expandAst(NodeTypes.BLOCK)
      blocks.checkNodeCount(3)

      def tryBlock = blocks.filter(PropertyNames.ORDER, 1)
      tryBlock.checkNodeCount(1)

      def open = tryBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "open()")
      open.checkNodeCount(1)

      def catchBlock = blocks.filter(PropertyNames.ORDER, 2)
      catchBlock.checkNodeCount(1)

      def handle =
        catchBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "handle()")
      handle.checkNodeCount(1)

      def finallyBlock = blocks.filter(PropertyNames.ORDER, 3)
      finallyBlock.checkNodeCount(1)

      def close = finallyBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "close()")
      close.checkNodeCount(1)
    }

    "have correct structure for 1 object with simple values" in AstFixture("""
       |var x = {
       | key1: "value",
       | key2: 2,
       | ...rest
       |}
       |""".stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head

      def block = assignment.expandAst(NodeTypes.BLOCK)
      checkObjectInitialization(block.head, ("key1", "\"value\""))
      checkObjectInitialization(block.head, ("key2", "2"))
      // TODO: SpreadElement is not handled yet. It is put there as UNKNOWN.
      checkObjectInitialization(block.head, ("rest", "...rest"))
    }

    "have correct structure for 1 object with computed values" in AstFixture("""
       |var x = {
       | key1: value(),
       | key2: foo.compute()
       |}
       |""".stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head

      def block = assignment.expandAst(NodeTypes.BLOCK)
      checkObjectInitialization(block.head, ("key1", "value()"))
      checkObjectInitialization(block.head, ("key2", "foo.compute()"))
    }

    "have correct structure for 1 object with object function" in AstFixture("""
       |var x = {
       | key1: value(),
       | foo() {}
       |}
       |""".stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head

      def block = assignment.expandAst(NodeTypes.BLOCK)
      checkObjectInitialization(block.head, ("key1", "value()"))
      checkObjectInitialization(block.head, ("foo", "foo")) // ref to foo
    }

    "have correct structure for object with computed property name" ignore AstFixture("""
        |var x = {
        | [ 1 + 1 ]: value()
        |}
        |""".stripMargin) { _ => }

    "have correct structure for conditional expression" in AstFixture("x ? y : z;") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def block = program.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def blockMethodReturn = program.expandAst(NodeTypes.METHOD_RETURN)
      blockMethodReturn.checkNodeCount(1)

      def call = block.expandAst(NodeTypes.CALL)
      call.checkNodeCount(1)
      call.checkProperty(PropertyNames.CODE, "x ? y : z")
      call.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.conditional)

      def identifiers = call.expandAst(NodeTypes.IDENTIFIER)
      identifiers.checkNodeCount(3)

      def identifierZ = identifiers.filter(PropertyNames.NAME, "z")
      identifierZ.checkNodeCount(1)
      identifierZ.checkProperty(PropertyNames.NAME, "z")

      def identifierX = identifiers.filter(PropertyNames.NAME, "x")
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def identifierY = identifiers.filter(PropertyNames.NAME, "y")
      identifierY.checkNodeCount(1)
      identifierY.checkProperty(PropertyNames.NAME, "y")
    }

    "have correct file name for empty file" in AstFixture("function method(x) {}") { cpg =>
      val List(file, tps) = cpg.file.toList
      file.name should endWith("code.js")
      tps.name shouldBe "builtintypes"
    }

    "have correct name space block for empty file" in AstFixture("") { cpg =>
      val List(file, tps) = cpg.file.toList
      file.name should endWith("code.js")
      tps.name shouldBe "builtintypes"

      val List(ns, _) = cpg.namespaceBlock.toList
      ns.name shouldBe Defines.GLOBAL_NAMESPACE
      ns.fullName should endWith("code.js:" + Defines.GLOBAL_NAMESPACE)
      ns.order shouldBe 1
      Traversal(ns).expand(EdgeTypes.SOURCE_FILE).head shouldBe file
    }

    "have :program method correctly attached to files namespace block" in AstFixture("") { cpg =>
      val List(file, tps) = cpg.file.toList
      file.name should endWith("code.js")
      tps.name shouldBe "builtintypes"

      val List(ns, _) = cpg.namespaceBlock.toList
      ns.name shouldBe Defines.GLOBAL_NAMESPACE
      ns.fullName should endWith("code.js:" + Defines.GLOBAL_NAMESPACE)
      ns.order shouldBe 1
      ns.method.head.name shouldBe ":program"
    }

    "have correct structure for empty method nested in top level method" in AstFixture("function method(x) {}") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodMethod = method.expandAst(NodeTypes.METHOD)
      methodMethod.checkNodeCount(1)

      def virtualModifier = methodMethod.expandAst(NodeTypes.MODIFIER)
      virtualModifier.checkNodeCount(1)
      virtualModifier.checkProperty(PropertyNames.MODIFIER_TYPE, ModifierTypes.VIRTUAL)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def assignment = block.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)
      assignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def localForMethod = block.expandAst(NodeTypes.LOCAL)
      localForMethod.checkNodeCount(1)
      localForMethod.checkProperty(PropertyNames.NAME, "method")

      def methodIdentifier =
        assignment.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      methodIdentifier.checkNodeCount(1)
      methodIdentifier.checkProperty(PropertyNames.NAME, "method")

      methodIdentifier.expandRef().head shouldBe localForMethod.head
    }

    "have correct parameter order in lambda function with ignored param" in AstFixture("var x = ([, param]) => param") {
      cpg =>
        def lambdaFullName = "code.js::program:anonymous"
        def lambda         = cpg.method.fullNameExact(lambdaFullName)
        lambda.checkNodeCount(1)

        def lambdaBlock = lambda.expandAst(NodeTypes.BLOCK)
        lambdaBlock.checkNodeCount(1)

        def parameters = lambda.expandAst(NodeTypes.METHOD_PARAMETER_IN)
        parameters.checkNodeCount(2)

        def param1 = parameters.filter(PropertyNames.INDEX, 0)
        param1.checkNodeCount(1)
        param1.checkProperty(PropertyNames.NAME, "this")
        param1.checkProperty(PropertyNames.CODE, "this")

        def param2 = parameters.filter(PropertyNames.INDEX, 1)
        param2.checkNodeCount(1)
        param2.checkProperty(PropertyNames.NAME, "param1_0")
        param2.checkProperty(PropertyNames.CODE, "[, param]")

        def param2Local = lambdaBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "param")
        param2Local.checkNodeCount(1)

        def assignmentToId =
          lambdaBlock
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.CODE, "param = param1_0.param")
        assignmentToId.checkNodeCount(1)
    }

    "have two lambda functions in same scope level with different full names" in AstFixture("""
          | var x = (a) => a
          | var y = (b) => b
        """.stripMargin) { cpg =>
      def lambda1FullName = "code.js::program:anonymous"
      def lambda2FullName = "code.js::program:anonymous1"

      def lambda1 = cpg.method.fullNameExact(lambda1FullName)
      lambda1.checkNodeCount(1)

      def lambda2 = cpg.method.fullNameExact(lambda2FullName)
      lambda2.checkNodeCount(1)

      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def assignment1 = block.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 1)
      assignment1.checkNodeCount(1)
      assignment1.checkProperty(PropertyNames.NAME, Operators.assignment)

      def lambda1MethodRef = assignment1.expandAst(NodeTypes.METHOD_REF)
      lambda1MethodRef.checkNodeCount(1)
      lambda1MethodRef.checkProperty(PropertyNames.METHOD_FULL_NAME, lambda1FullName)

      def assignment2 = block.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
      assignment2.checkNodeCount(1)
      assignment2.checkProperty(PropertyNames.NAME, Operators.assignment)

      def lambda2MethodRef = assignment2.expandAst(NodeTypes.METHOD_REF)
      lambda2MethodRef.checkNodeCount(1)
      lambda2MethodRef.checkProperty(PropertyNames.METHOD_FULL_NAME, lambda2FullName)
    }

    "be correct for call expression" in AstFixture("""
         |function method(x) {
         |  foo(x);
         |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def fooCall = block.expandAst(NodeTypes.CALL)
      fooCall.checkNodeCount(1)
      fooCall.checkProperty(PropertyNames.CODE, "foo(x)")
      fooCall.checkProperty(PropertyNames.NAME, "")
      fooCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.DYNAMIC_DISPATCH)

      def receiver = fooCall.expandReceiver()
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.NAME, "foo")
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def argumentThis = fooCall.expandAst().filter(PropertyNames.NAME, "this")
      argumentThis.checkNodeCount(1)
      argumentThis.checkProperty(PropertyNames.ORDER, 1)
      argumentThis.checkProperty(PropertyNames.ARGUMENT_INDEX, 0)

      def argument1 = fooCall.expandAst().filter(PropertyNames.NAME, "x")
      argument1.checkNodeCount(1)
      argument1.checkProperty(PropertyNames.ORDER, 2)
      argument1.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
    }

    "be correct for chained calls" in AstFixture("x.foo(y).bar(z)") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def barCall = block.expandAst(NodeTypes.CALL)
      barCall.checkNodeCount(1)
      barCall.checkProperty(PropertyNames.CODE, "x.foo(y).bar(z)")
      barCall.checkProperty(PropertyNames.NAME, "")

      def receiver       = barCall.expandReceiver(NodeTypes.CALL)
      def receiverViaAst = barCall.expandAst(NodeTypes.CALL)
      receiver.checkNodeCount(1)
      receiver.head shouldBe receiverViaAst.head
      receiver.checkProperty(PropertyNames.CODE, "(_tmp_0 = x.foo(y)).bar")
      receiver.checkProperty(PropertyNames.NAME, Operators.fieldAccess)
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def barIdentifier = receiver.expandAst(NodeTypes.FIELD_IDENTIFIER)
      barIdentifier.checkNodeCount(1)
      barIdentifier.checkProperty(PropertyNames.CANONICAL_NAME, "bar")
      barIdentifier.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

      def tmpAssignment = receiver.expandAst(NodeTypes.CALL)
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.CODE, "(_tmp_0 = x.foo(y))")
      tmpAssignment.checkProperty(PropertyNames.NAME, "<operator>.assignment")
      tmpAssignment.checkProperty(PropertyNames.ORDER, 1)

      def tmpIdentifier = tmpAssignment.expandAst(NodeTypes.IDENTIFIER)
      tmpIdentifier.checkNodeCount(1)
      tmpIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
      tmpIdentifier.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def barBaseTree = tmpAssignment.expandAst(NodeTypes.CALL)
      barBaseTree.checkNodeCount(1)
      barBaseTree.checkProperty(PropertyNames.CODE, "x.foo(y)")
      barBaseTree.checkProperty(PropertyNames.NAME, "")
      barBaseTree.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

      // barBaseTree constructs is tested for in another test.

      def thisArg = barCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      thisArg.checkNodeCount(1)
      thisArg.checkProperty(PropertyNames.NAME, "_tmp_0")
      thisArg.checkProperty(PropertyNames.ORDER, 1)

      def zArg = barCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      zArg.checkNodeCount(1)
      zArg.checkProperty(PropertyNames.NAME, "z")
      zArg.checkProperty(PropertyNames.ORDER, 2)
    }

    "be correct for call on object" in AstFixture("""
          |function method(x) {
          |  x.foo();
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def fooCall = block.expandAst(NodeTypes.CALL)
      fooCall.checkNodeCount(1)
      fooCall.checkProperty(PropertyNames.CODE, "x.foo()")
      fooCall.checkProperty(PropertyNames.NAME, "")
      fooCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.DYNAMIC_DISPATCH)

      def receiver = fooCall.expandAst(NodeTypes.CALL)
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.CODE, "x.foo")
      receiver.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def base = receiver.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      base.checkNodeCount(1)
      base.checkProperty(PropertyNames.NAME, "x")

      def accessElement =
        receiver.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      accessElement.checkNodeCount(1)
      accessElement.checkProperty(PropertyNames.CANONICAL_NAME, "foo")
    }

    "have block for while body for while statement with brackets" in AstFixture("while (x < 0) {}") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def whileNode = block.expandAst(NodeTypes.CONTROL_STRUCTURE)
      whileNode.checkNodeCount(1)
      whileNode.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.WHILE)

      def whileBlock = whileNode.expandAst(NodeTypes.BLOCK)
      whileBlock.checkNodeCount(1)
    }

    "have no block for while body for while statement without brackets" in AstFixture("""
          |while (x < 0)
          |  x += 1
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def whileNode = block.expandAst(NodeTypes.CONTROL_STRUCTURE)
      whileNode.checkNodeCount(1)
      whileNode.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.WHILE)

      def whileBlock = whileNode.expandAst(NodeTypes.BLOCK)
      whileBlock.checkNodeCount(0)
    }

    "have local variable for function with correct type full name" in AstFixture("function method(x) {}") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def block = method.block
      block.checkNodeCount(1)

      val localFoo = block.local.head
      localFoo.name shouldBe "method"
      localFoo.typeFullName should endWith("code.js::program:method")
    }

    "have corresponding type decl with correct bindings for function" in AstFixture("function method(x) {}") { cpg =>
      def typeDecl = cpg.typeDecl.nameExact("method")
      typeDecl.checkNodeCount(1)
      typeDecl.fullName.head should endWith("code.js::program:method")

      def binding = typeDecl.expand(EdgeTypes.BINDS, NodeTypes.BINDING)
      binding.checkNodeCount(1)
      binding.checkProperty(PropertyNames.NAME, "")
      binding.checkProperty(PropertyNames.SIGNATURE, "")

      def boundMethod = binding.expandRef(NodeTypes.METHOD)
      boundMethod.checkNodeCount(1)
      boundMethod.head shouldBe cpg.method.nameExact("method").head
    }

    "have correct structure for empty method" in AstFixture("function method(x) {}") { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      method.expandAst(NodeTypes.BLOCK).checkNodeCount(1)

      method
        .expandAst(NodeTypes.METHOD_RETURN)
        .checkNodeCount(1)
        .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")

      method
        .expandAst(NodeTypes.METHOD_PARAMETER_IN)
        .filter(PropertyNames.INDEX, 0)
        .checkNodeCount(1)
        .checkProperty(PropertyNames.NAME, "this")
        .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")

      method
        .expandAst(NodeTypes.METHOD_PARAMETER_IN)
        .filter(PropertyNames.INDEX, 1)
        .checkNodeCount(1)
        .checkProperty(PropertyNames.NAME, "x")
        .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")
    }

    "have correct structure for empty method with rest parameter" in AstFixture("function method(x, ...args) {}") {
      cpg =>
        def method = cpg.method.nameExact("method")
        method.checkNodeCount(1)

        method.expandAst(NodeTypes.BLOCK).checkNodeCount(1)

        method
          .expandAst(NodeTypes.METHOD_RETURN)
          .checkNodeCount(1)
          .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")

        method
          .expandAst(NodeTypes.METHOD_PARAMETER_IN)
          .filter(PropertyNames.INDEX, 0)
          .checkNodeCount(1)
          .checkProperty(PropertyNames.NAME, "this")
          .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")

        method
          .expandAst(NodeTypes.METHOD_PARAMETER_IN)
          .filter(PropertyNames.INDEX, 1)
          .checkNodeCount(1)
          .checkProperty(PropertyNames.NAME, "x")
          .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")

        method
          .expandAst(NodeTypes.METHOD_PARAMETER_IN)
          .filter(PropertyNames.INDEX, 2)
          .checkNodeCount(1)
          .checkProperty(PropertyNames.NAME, "args")
          .checkProperty(PropertyNames.CODE, "...args")
          .checkProperty(PropertyNames.TYPE_FULL_NAME, "ANY")
          .checkProperty(PropertyNames.IS_VARIADIC, true)
    }

    "have correct structure for decl assignment" in AstFixture("function foo(x) { var local = 1; }") { cpg =>
      def method = cpg.method.nameExact("foo")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def parameterThis =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.INDEX, 0)
      parameterThis.checkNodeCount(1)
      parameterThis.checkProperty(PropertyNames.NAME, "this")

      def parameterX =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.INDEX, 1)
      parameterX.checkNodeCount(1)
      parameterX.checkProperty(PropertyNames.NAME, "x")

      def local = block.expandAst(NodeTypes.LOCAL)
      local.checkNodeCount(1)
      local.checkProperty(PropertyNames.NAME, "local")

      def assignmentCall = block.expandAst(NodeTypes.CALL)
      assignmentCall.checkNodeCount(1)

      def assignmentOut = assignmentCall.expandAst(NodeTypes.IDENTIFIER)
      assignmentOut.checkNodeCount(1)
      assignmentOut.checkProperty(PropertyNames.NAME, "local")
    }

    "have correct structure for decl assignment with identifier on right hand side" in AstFixture(
      "function foo(x) { var local = x; }"
    ) { cpg =>
      def method = cpg.method.nameExact("foo")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def parameterThis =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.INDEX, 0)
      parameterThis.checkNodeCount(1)
      parameterThis.checkProperty(PropertyNames.NAME, "this")

      def parameterX =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.INDEX, 1)
      parameterX.checkNodeCount(1)
      parameterX.checkProperty(PropertyNames.NAME, "x")

      def local = block.expandAst(NodeTypes.LOCAL)
      local.checkNodeCount(1)
      local.checkProperty(PropertyNames.NAME, "local")

      def assignmentCall = block.expandAst(NodeTypes.CALL)
      assignmentCall.checkNodeCount(1)

      def assingmentsOut = assignmentCall.expandAst(NodeTypes.IDENTIFIER)
      assingmentsOut.checkNodeCount(2)

      def outLocal = assingmentsOut.filter(PropertyNames.NAME, "local")
      outLocal.checkNodeCount(1)

      def outRight = assingmentsOut.filter(PropertyNames.NAME, "x")
      outRight.checkNodeCount(1)
    }

    "have correct structure for decl assignment of multiple locals" in AstFixture(
      "function foo(x,y) { var local1 = x; var local2 = y; }"
    ) { cpg =>
      def method = cpg.method.nameExact("foo")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodParamIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      methodParamIn.checkNodeCount(3)

      def thisIn = methodParamIn.filter(PropertyNames.NAME, "this")
      thisIn.checkNodeCount(1)

      def firstIn = methodParamIn.filter(PropertyNames.NAME, "x")
      firstIn.checkNodeCount(1)

      def secondIn = methodParamIn.filter(PropertyNames.NAME, "y")
      secondIn.checkNodeCount(1)

      def locals = block.expandAst(NodeTypes.LOCAL)
      locals.checkNodeCount(2)

      def firstLocal = locals.filter(PropertyNames.NAME, "local1")
      firstLocal.checkNodeCount(1)

      def secondLocal = locals.filter(PropertyNames.NAME, "local2")
      secondLocal.checkNodeCount(1)

      def assignmentCalls = block.expandAst(NodeTypes.CALL)
      assignmentCalls.checkNodeCount(2)

      def firstAssigment = assignmentCalls.filter(PropertyNames.CODE, "local1 = x")
      firstAssigment.checkNodeCount(1)
      def secondAssigment = assignmentCalls.filter(PropertyNames.CODE, "local2 = y")
      secondAssigment.checkNodeCount(1)

      def out1 = firstAssigment.expandAst(NodeTypes.IDENTIFIER)
      out1.checkNodeCount(2)

      def outLocal1 = out1.filter(PropertyNames.NAME, "local1")
      outLocal1.checkNodeCount(1)

      def outRight1 = out1.filter(PropertyNames.NAME, "x")
      outRight1.checkNodeCount(1)

      def out2 = secondAssigment.expandAst(NodeTypes.IDENTIFIER)
      out2.checkNodeCount(2)

      def outLocal2 = out2.filter(PropertyNames.NAME, "local2")
      outLocal2.checkNodeCount(1)

      def outRight2 = out2.filter(PropertyNames.NAME, "y")
      outRight2.checkNodeCount(1)
    }

    "be correct for nested expression" in AstFixture("function method() { var x; var y; var z; x = y + z; }") { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def block = method.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def assignmentCall = block.expandAst(NodeTypes.CALL)
      assignmentCall.checkNodeCount(1)

      def identifierX = assignmentCall.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def plus = assignmentCall.expandAst(NodeTypes.CALL)
      plus.checkNodeCount(1)

      def identifiers = plus.expandAst(NodeTypes.IDENTIFIER)
      identifiers.checkNodeCount(2)

      def identifierY = identifiers.filter(PropertyNames.NAME, "y")
      identifierY.checkNodeCount(1)

      def identifierZ = identifiers.filter(PropertyNames.NAME, "z")
      identifierZ.checkNodeCount(1)
    }

    "be correct for while loop" in AstFixture("""
          |function method(x) {
          |  while (x < 1) {
          |    x += 1;
          |  }
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def parameterIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      parameterIn.checkNodeCount(2)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def whileNode = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      whileNode
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.WHILE)
        .checkProperty(PropertyNames.ORDER, 1)

      def whileCondition = whileNode.expandAst(NodeTypes.CALL)
      whileCondition
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x < 1")
        .checkProperty(PropertyNames.ORDER, 1)

      def whileBlock = whileNode.expandAst(NodeTypes.BLOCK)
      whileBlock
        .checkNodeCount(1)
        .checkProperty(PropertyNames.ORDER, 2)

      def assign = whileBlock.expandAst(NodeTypes.CALL)
      assign
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x += 1")
        .checkProperty(PropertyNames.ORDER, 1)

      def identifierX = assign.expandAst(NodeTypes.IDENTIFIER)
      identifierX
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x")
        .checkProperty(PropertyNames.ORDER, 1)

      def literal1 = assign.expandAst(NodeTypes.LITERAL)
      literal1
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "1")
        .checkProperty(PropertyNames.ORDER, 2)
    }

    "be correct for if" in AstFixture("""
          |function method(x) {
          |  var y;
          |  if (x > 0)
          |    y = 0;
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def parameterIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      parameterIn.checkNodeCount(2)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localY = methodBlock.expandAst(NodeTypes.LOCAL)
      localY.checkNodeCount(1)

      def ifNode = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      ifNode
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.IF)
        .checkProperty(PropertyNames.ORDER, 1)

      def ifCondition = ifNode.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 1)
      ifCondition
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x > 0")

      def assignment = ifNode.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
      assignment
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "y = 0")

      def identifierY = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierY
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "y")
        .checkProperty(PropertyNames.ORDER, 1)

      def literal0 = assignment.expandAst(NodeTypes.LITERAL)
      literal0
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "0")
        .checkProperty(PropertyNames.ORDER, 2)
    }

    "be correct for if-else" in AstFixture("""
         |function method(x) {
         |  var y;
         |  if (x > 0) {
         |    y = 0;
         |  } else {
         |    y = 1;
         |  }
         |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def parameterIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      parameterIn.checkNodeCount(2)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localY = methodBlock.expandAst(NodeTypes.LOCAL)
      localY.checkNodeCount(1)
      localY.checkProperty(PropertyNames.ORDER, 0)

      def ifNode = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      ifNode
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.IF)
        .checkProperty(PropertyNames.ORDER, 1)

      def ifCondition = ifNode.expandAst(NodeTypes.CALL)
      ifCondition
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x > 0")
        .checkProperty(PropertyNames.ORDER, 1)

      def ifBlock = ifNode.expandAst(NodeTypes.BLOCK).filter(PropertyNames.ORDER, 2)
      ifBlock.checkNodeCount(1)

      def ifBlockAssign = ifBlock.expandAst(NodeTypes.CALL)
      ifBlockAssign
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "y = 0")
        .checkProperty(PropertyNames.ORDER, 1)
      // We do not go further into AST of "y = 0" because we have tests that already.

      def elseBlock = ifNode.expandAst(NodeTypes.BLOCK).filter(PropertyNames.ORDER, 3)
      elseBlock.checkNodeCount(1)

      def elseBlockAssign = elseBlock.expandAst(NodeTypes.CALL)
      elseBlockAssign
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "y = 1")
        .checkProperty(PropertyNames.ORDER, 1)
    // We do not go further into AST of "y = 1" because we have tests that already.
    }

    "be correct for for-loop with for-in" in AstFixture("""
          |for (var i in arr) {
          |   foo(i)
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def loopBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      checkForInOrOf(loopBlock.head)
    }

    "be correct for for-loop with for-of" in AstFixture("""
          |for (var i of arr) {
          |   foo(i)
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def loopBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      checkForInOrOf(loopBlock.head)
    }

    "be correct for for-loop with empty test" in AstFixture("for(;;){}") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def forNode = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      forNode
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.FOR)
        .checkProperty(PropertyNames.ORDER, 1)

      def forCondition = forNode.expandAst(NodeTypes.LITERAL).filter(PropertyNames.ORDER, 2)
      forCondition
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "true")
    }

    "be correct for for-loop" in AstFixture("""
          |function method(x,y) {
          |  for (x = 0; x < 1; x += 1) {
          |    z = 0;
          |  }
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def parameterInX =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.ORDER, 1)
      parameterInX
        .checkNodeCount(1)
        .checkProperty(PropertyNames.NAME, "x")

      def parameterInY =
        method.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.ORDER, 2)
      parameterInY
        .checkNodeCount(1)
        .checkProperty(PropertyNames.NAME, "y")

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def forNode = methodBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
      forNode
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.FOR)
        .checkProperty(PropertyNames.ORDER, 1)

      def forInit = forNode.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 1)
      forInit
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x = 0")

      def forCondition = forNode.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
      forCondition
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x < 1")

      def forModify = forNode.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 3)
      forModify
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "x += 1")

      def forBlock = forNode.expandAst(NodeTypes.BLOCK)
      forBlock
        .checkNodeCount(1)
        .checkProperty(PropertyNames.ORDER, 4)

      def forBlockAssign = forBlock.expandAst(NodeTypes.CALL)
      forBlockAssign
        .checkNodeCount(1)
        .checkProperty(PropertyNames.CODE, "z = 0")
        .checkProperty(PropertyNames.ORDER, 1)
    }

    "handle switch statements and" should {
      "be correct for switch with one case" in AstFixture("switch (x) { case 1: y; }") { cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
        switch.checkNodeCount(1)
        switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

        def switchExpr = switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
        switchExpr.checkNodeCount(1)
        switchExpr.checkProperty(PropertyNames.ORDER, 1)
        switchExpr.checkProperty(PropertyNames.CODE, "x")

        def switchBlock = switch.expandAst(NodeTypes.BLOCK)
        switchBlock.checkNodeCount(1)

        def caseLabel =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 1:")
        caseLabel.checkNodeCount(1)
        caseLabel.checkProperty(PropertyNames.ORDER, 1)

        def caseExpr = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        caseExpr.checkNodeCount(1)
        caseExpr.checkProperty(PropertyNames.ORDER, 2)

        def identifierY =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "y")
        identifierY.checkNodeCount(1)
        identifierY.checkProperty(PropertyNames.ORDER, 3)
      }

      "be correct for switch with multiple cases" in AstFixture("switch (x) { case 1: y; case 2: z; }") { cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
        switch.checkNodeCount(1)
        switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

        def switchExpr =
          switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
        switchExpr.checkNodeCount(1)
        switchExpr.checkProperty(PropertyNames.ORDER, 1)
        switchExpr.checkProperty(PropertyNames.CODE, "x")

        def switchBlock = switch.expandAst(NodeTypes.BLOCK)
        switchBlock.checkNodeCount(1)

        def caseLabel1 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 1:")
        caseLabel1.checkNodeCount(1)
        caseLabel1.checkProperty(PropertyNames.ORDER, 1)

        def caseExpr1 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        caseExpr1.checkNodeCount(1)
        caseExpr1.checkProperty(PropertyNames.ORDER, 2)

        def identifierY =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "y")
        identifierY.checkNodeCount(1)
        identifierY.checkProperty(PropertyNames.ORDER, 3)

        def caseLabel2 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 2:")
        caseLabel2.checkNodeCount(1)
        caseLabel2.checkProperty(PropertyNames.ORDER, 4)

        def caseExpr2 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
        caseExpr2.checkNodeCount(1)
        caseExpr2.checkProperty(PropertyNames.ORDER, 5)

        def identifierZ =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "z")
        identifierZ.checkNodeCount(1)
        identifierZ.checkProperty(PropertyNames.ORDER, 6)
      }

      "be correct for switch with multiple cases on same spot" in AstFixture("switch (x) { case 1: case 2: y; }") {
        cpg =>
          def program = cpg.method.nameExact(":program")
          program.checkNodeCount(1)

          def programBlock = program.expandAst(NodeTypes.BLOCK)
          programBlock.checkNodeCount(1)

          def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
          switch.checkNodeCount(1)
          switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

          def switchExpr =
            switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
          switchExpr.checkNodeCount(1)
          switchExpr.checkProperty(PropertyNames.ORDER, 1)
          switchExpr.checkProperty(PropertyNames.CODE, "x")

          def switchBlock = switch.expandAst(NodeTypes.BLOCK)
          switchBlock.checkNodeCount(1)

          def caseLabel1 =
            switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 1:")
          caseLabel1.checkNodeCount(1)
          caseLabel1.checkProperty(PropertyNames.ORDER, 1)

          def caseExpr1 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
          caseExpr1.checkNodeCount(1)
          caseExpr1.checkProperty(PropertyNames.ORDER, 2)

          def caseLabel2 =
            switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 2:")
          caseLabel2.checkNodeCount(1)
          caseLabel2.checkProperty(PropertyNames.ORDER, 3)

          def caseExpr2 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
          caseExpr2.checkNodeCount(1)
          caseExpr2.checkProperty(PropertyNames.ORDER, 4)

          def identifierY =
            switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "y")
          identifierY.checkNodeCount(1)
          identifierY.checkProperty(PropertyNames.ORDER, 5)
      }

      "be correct for switch with multiple cases and multiple cases on same spot" in AstFixture(
        "switch (x) { case 1: case 2: y; case 3: z; }"
      ) { cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
        switch.checkNodeCount(1)
        switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

        def switchExpr =
          switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
        switchExpr.checkNodeCount(1)
        switchExpr.checkProperty(PropertyNames.ORDER, 1)
        switchExpr.checkProperty(PropertyNames.CODE, "x")

        def switchBlock = switch.expandAst(NodeTypes.BLOCK)
        switchBlock.checkNodeCount(1)

        def caseLabel1 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 1:")
        caseLabel1.checkNodeCount(1)
        caseLabel1.checkProperty(PropertyNames.ORDER, 1)

        def caseExpr1 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        caseExpr1.checkNodeCount(1)
        caseExpr1.checkProperty(PropertyNames.ORDER, 2)

        def caseLabel2 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 2:")
        caseLabel2.checkNodeCount(1)
        caseLabel2.checkProperty(PropertyNames.ORDER, 3)

        def caseExpr2 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
        caseExpr2.checkNodeCount(1)
        caseExpr2.checkProperty(PropertyNames.ORDER, 4)

        def identifierY =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "y")
        identifierY.checkNodeCount(1)
        identifierY.checkProperty(PropertyNames.ORDER, 5)

        def caseLabel3 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 3:")
        caseLabel3.checkNodeCount(1)
        caseLabel3.checkProperty(PropertyNames.ORDER, 6)

        def caseExpr3 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "3")
        caseExpr3.checkNodeCount(1)
        caseExpr3.checkProperty(PropertyNames.ORDER, 7)

        def identifierZ =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.CODE, "z")
        identifierZ.checkNodeCount(1)
        identifierZ.checkProperty(PropertyNames.ORDER, 8)
      }

      "be correct for switch with default case" in AstFixture("switch (x) { default: y; }") { cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
        switch.checkNodeCount(1)
        switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

        def switchExpr =
          switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
        switchExpr.checkNodeCount(1)
        switchExpr.checkProperty(PropertyNames.ORDER, 1)
        switchExpr.checkProperty(PropertyNames.CODE, "x")

        programBlock.expandAst(NodeTypes.LITERAL).checkNodeCount(0)

        def switchBlock = switch.expandAst(NodeTypes.BLOCK)
        switchBlock.checkNodeCount(1)

        def caseLabel =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "default:")
        caseLabel.checkNodeCount(1)
        caseLabel.checkProperty(PropertyNames.ORDER, 1)

        def identifierY =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "y")
        identifierY.checkNodeCount(1)
        identifierY.checkProperty(PropertyNames.ORDER, 2)
      }

      "be correct for switch with case and default combined" in AstFixture(
        "switch (x) { case 1: y; break; default: z; }"
      ) { cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def switch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
        switch.checkNodeCount(1)
        switch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

        def switchExpr =
          switch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
        switchExpr.checkNodeCount(1)
        switchExpr.checkProperty(PropertyNames.ORDER, 1)
        switchExpr.checkProperty(PropertyNames.CODE, "x")

        def switchBlock = switch.expandAst(NodeTypes.BLOCK)
        switchBlock.checkNodeCount(1)

        def caseLabel1 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "case 1:")
        caseLabel1.checkNodeCount(1)
        caseLabel1.checkProperty(PropertyNames.ORDER, 1)

        def caseExpr1 = switchBlock.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        caseExpr1.checkNodeCount(1)
        caseExpr1.checkProperty(PropertyNames.ORDER, 2)

        def identifierY =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "y")
        identifierY.checkNodeCount(1)
        identifierY.checkProperty(PropertyNames.ORDER, 3)

        def break =
          switchBlock
            .expandAst(NodeTypes.CONTROL_STRUCTURE)
            .filter(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.BREAK)
        break.checkNodeCount(1)
        break.checkProperty(PropertyNames.ORDER, 4)

        def caseLabel2 =
          switchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "default:")
        caseLabel2.checkNodeCount(1)
        caseLabel2.checkProperty(PropertyNames.ORDER, 5)

        def identifierZ =
          switchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "z")
        identifierZ.checkNodeCount(1)
        identifierZ.checkProperty(PropertyNames.ORDER, 6)
      }

      "be correct for switch with nested switch" in AstFixture("switch (x) { default: switch(y) { default: z; } }") {
        cpg =>
          def program = cpg.method.nameExact(":program")
          program.checkNodeCount(1)

          def programBlock = program.expandAst(NodeTypes.BLOCK)
          programBlock.checkNodeCount(1)

          def topLevelSwitch = programBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
          topLevelSwitch.checkNodeCount(1)
          topLevelSwitch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

          def topLevelSwitchExpr =
            topLevelSwitch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
          topLevelSwitchExpr.checkNodeCount(1)
          topLevelSwitchExpr.checkProperty(PropertyNames.ORDER, 1)
          topLevelSwitchExpr.checkProperty(PropertyNames.CODE, "x")

          def topLevelSwitchBlock = topLevelSwitch.expandAst(NodeTypes.BLOCK)
          topLevelSwitchBlock.checkNodeCount(1)

          def topLevelCaseLabel =
            topLevelSwitchBlock
              .expandAst(NodeTypes.JUMP_TARGET)
              .filter(PropertyNames.CODE, "default:")
          topLevelCaseLabel.checkNodeCount(1)
          topLevelCaseLabel.checkProperty(PropertyNames.ORDER, 1)

          def nestedSwitch = topLevelSwitchBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
          nestedSwitch.checkNodeCount(1)
          nestedSwitch.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.SWITCH)

          def nestedSwitchExpr =
            nestedSwitch.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "y")
          nestedSwitchExpr.checkNodeCount(1)
          nestedSwitchExpr.checkProperty(PropertyNames.ORDER, 1)
          nestedSwitchExpr.checkProperty(PropertyNames.CODE, "y")

          def nestedSwitchBlock = nestedSwitch.expandAst(NodeTypes.BLOCK)
          nestedSwitchBlock.checkNodeCount(1)

          def nestedCaseLabel =
            nestedSwitchBlock.expandAst(NodeTypes.JUMP_TARGET).filter(PropertyNames.CODE, "default:")
          nestedCaseLabel.checkNodeCount(1)
          nestedCaseLabel.checkProperty(PropertyNames.ORDER, 1)

          def identifierZ =
            nestedSwitchBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "z")
          identifierZ.checkNodeCount(1)
          identifierZ.checkProperty(PropertyNames.ORDER, 2)
      }
    }

    "be correct for logical expression '++'" in AstFixture("""
           |function method(x) {
           |  true && false;
           |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def logicalCall = method.expandAst(NodeTypes.BLOCK).expandAst(NodeTypes.CALL)
      logicalCall.checkNodeCount(1)
      logicalCall.checkProperty(PropertyNames.NAME, Operators.logicalAnd)

      def callArg1 = logicalCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.ARGUMENT_INDEX, 1)
      callArg1.checkNodeCount(1)
      callArg1.checkProperty(PropertyNames.CODE, "true")

      def callArg2 = logicalCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.ARGUMENT_INDEX, 2)
      callArg2.checkNodeCount(1)
      callArg2.checkProperty(PropertyNames.CODE, "false")
    }

    "be correct for unary expression '++'" in AstFixture("""
         |function method(x) {
         |  ++x;
         |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def parameterIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      parameterIn.checkNodeCount(2)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def unaryPlusCall = methodBlock.expandAst(NodeTypes.CALL)
      unaryPlusCall.checkNodeCount(1)
      unaryPlusCall.checkProperty(PropertyNames.CODE, "++x")

      def identifierX = unaryPlusCall.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")
    }

    "be correct for member access used in an assignment (direct)" in AstFixture("""
          |function method(x) {
          |  z = x.a;
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)
      assignment.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.assignment)

      def identifierZ = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierZ.checkNodeCount(1)
      identifierZ.checkProperty(PropertyNames.NAME, "z")

      def rightHandSide = assignment.expandAst(NodeTypes.CALL)
      rightHandSide.checkNodeCount(1)
      rightHandSide.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierRightX =
        rightHandSide.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      identifierRightX.checkNodeCount(1)
      identifierRightX.checkProperty(PropertyNames.NAME, "x")
      identifierRightX.checkProperty(PropertyNames.CODE, "x")
      def identifierRightA =
        rightHandSide.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      identifierRightA.checkNodeCount(1)
      identifierRightA.checkProperty(PropertyNames.CANONICAL_NAME, "a")
      identifierRightA.checkProperty(PropertyNames.CODE, "a")
    }

    "be correct for member access used in an assignment (chained)" in AstFixture("""
         |function method(x) {
         |  z = x.a.b.c;
         |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)
      assignment.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.assignment)

      def identifierZ = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierZ.checkNodeCount(1)
      identifierZ.checkProperty(PropertyNames.NAME, "z")

      def rightC = assignment.expandAst(NodeTypes.CALL)
      rightC.checkNodeCount(1)
      rightC.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierC = rightC.expandAst(NodeTypes.FIELD_IDENTIFIER)
      identifierC.checkNodeCount(1)
      identifierC.checkProperty(PropertyNames.CANONICAL_NAME, "c")

      def rightB = rightC.expandAst(NodeTypes.CALL)
      rightB.checkNodeCount(1)
      rightB.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierB = rightB.expandAst(NodeTypes.FIELD_IDENTIFIER)
      identifierB.checkNodeCount(1)
      identifierB.checkProperty(PropertyNames.CANONICAL_NAME, "b")

      def rightA = rightB.expandAst(NodeTypes.CALL)
      rightA.checkNodeCount(1)
      rightA.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierX =
        rightA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def identifierA =
        rightA.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      identifierA.checkNodeCount(1)
      identifierA.checkProperty(PropertyNames.CANONICAL_NAME, "a")
    }

    "be correct for member access used in an assignment (chained with method call)" in AstFixture("""
          |function method(x) {
          |  z = x.a.b.c();
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)
      assignment.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.assignment)

      def identifierZ = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierZ.checkNodeCount(1)
      identifierZ.checkProperty(PropertyNames.NAME, "z")

      def right = assignment.expandAst(NodeTypes.CALL)
      right.checkNodeCount(1)
      right.checkProperty(PropertyNames.NAME, "")

      def callToC = right.expandAst(NodeTypes.CALL)
      callToC.checkNodeCount(1)
      callToC.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierC =
        callToC.expandAst(NodeTypes.FIELD_IDENTIFIER)
      identifierC.checkNodeCount(1)
      identifierC.checkProperty(PropertyNames.CANONICAL_NAME, "c")

      def assignmentToTmp = callToC.expandAst(NodeTypes.CALL)
      assignmentToTmp.checkNodeCount(1)
      assignmentToTmp.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.assignment)

      def tmpIdentifier = assignmentToTmp.expandAst(NodeTypes.IDENTIFIER)
      tmpIdentifier.checkNodeCount(1)
      tmpIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")

      def fieldAccessXAB = assignmentToTmp.expandAst(NodeTypes.CALL)
      fieldAccessXAB.checkNodeCount(1)
      fieldAccessXAB.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierB =
        fieldAccessXAB.expandAst(NodeTypes.FIELD_IDENTIFIER)
      identifierB.checkNodeCount(1)
      identifierB.checkProperty(PropertyNames.CANONICAL_NAME, "b")

      def callToA = fieldAccessXAB.expandAst(NodeTypes.CALL)
      callToA.checkNodeCount(1)
      callToA.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierX =
        callToA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def identifierA =
        callToA.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      identifierA.checkNodeCount(1)
      identifierA.checkProperty(PropertyNames.CANONICAL_NAME, "a")
    }

    "be correct for member access used as return" in AstFixture("""
          |function method(x) {
          |  return x.a;
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def returnStatement = methodBlock.expandAst(NodeTypes.RETURN)
      returnStatement.checkNodeCount(1)

      def rightHandSide = returnStatement.expandAst(NodeTypes.CALL)
      rightHandSide.checkNodeCount(1)
      rightHandSide.checkProperty(PropertyNames.ORDER, 1)
      rightHandSide.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
      rightHandSide.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierX =
        rightHandSide.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def identifierA =
        rightHandSide.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      identifierA.checkNodeCount(1)
      identifierA.checkProperty(PropertyNames.CANONICAL_NAME, "a")
    }

    "be correct for member access as useless statement" in AstFixture("""
          |function method(x) {
          |  x.a;
          |}
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact("method")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def statement = methodBlock.expandAst(NodeTypes.CALL)
      statement.checkNodeCount(1)
      statement.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

      def identifierX =
        statement.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 1)
      identifierX.checkNodeCount(1)
      identifierX.checkProperty(PropertyNames.NAME, "x")

      def identifierA =
        statement.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ARGUMENT_INDEX, 2)
      identifierA.checkNodeCount(1)
      identifierA.checkProperty(PropertyNames.CANONICAL_NAME, "a")
    }

    "be correct for empty method" in AstFixture("function method() {}") { cpg =>
      def program = cpg.method.nameExact("method")
      program.checkNodeCount(1)

      def block = program.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def blockMethodReturn = program.expandAst(NodeTypes.METHOD_RETURN)
      blockMethodReturn.checkNodeCount(1)
    }

  }

  "AST method full names" should {
    "anonymous arrow function full name 1" in AstFixture("var func = (x) => x") { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:anonymous")
    }
    "anonymous arrow function full name 2" in AstFixture("this.func = (x) => x") { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:anonymous")
    }
    "anonymous function expression full name 1" in AstFixture("var func = function (x) {x}") { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:anonymous")
    }
    "anonymous function expression full name 2" in AstFixture("this.func = function (x) {x}") { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:anonymous")
    }
    "anonymous constructor full name 1" in AstFixture("class X { constructor(){} }") { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:X<constructor>")
    }
    "anonymous constructor of anonymous class full name" in AstFixture("""
                                                                          |var x = class {
                                                                          |  constructor(y) {
                                                                          |  }
                                                                          |}""".stripMargin) { cpg =>
      cpg.method.fullName.toSetMutable should contain("code.js::program:_anon_cdecl<constructor>")
    }
  }

  "AST variable scoping and linking" should {
    "have correct references for single local var" in AstFixture("""
         | var x
         | x = 1
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for single local let" in AstFixture("""
         | let x
         | x = 1
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for undeclared local" in AstFixture("x = 1") { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for undeclared local with 2 refs" in AstFixture("""
         | x = 1
         | x = 2
       """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment1 = methodBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 1)
      assignment1.checkNodeCount(1)

      def identifierX1 = assignment1.expandAst(NodeTypes.IDENTIFIER)
      identifierX1.checkNodeCount(1)

      def localXViaRef1 = identifierX1.expandRef(NodeTypes.LOCAL)
      localXViaRef1.head shouldBe localX.head

      def assignment2 = methodBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
      assignment2.checkNodeCount(1)

      def identifierX2 = assignment2.expandAst(NodeTypes.IDENTIFIER)
      identifierX2.checkNodeCount(1)

      def localXViaRef2 = identifierX2.expandRef(NodeTypes.LOCAL)
      localXViaRef2.head shouldBe localX.head
    }

    "have correct references for undeclared local in block" in AstFixture("""
        | {
        |   x = 1
        | }
       """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def nestedBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      nestedBlock.checkNodeCount(1)

      def assignment = nestedBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for single var in block" in AstFixture("""
        | {
        |   var x
        | }
        | x = 1
       """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def nestedBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = nestedBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for single post declared var" in AstFixture("""
         | x = 1
         | var x
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for single post declared var in block" in AstFixture("""
          | x = 1
          | {
          |   var x
          | }
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def nestedBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = nestedBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def assignment = methodBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for single nested access to let" in AstFixture("""
          | let x
          | {
          |   x = 1
          | }
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def localX = methodBlock.expandAst(NodeTypes.LOCAL)
      localX.checkNodeCount(1)

      def nestedBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      nestedBlock.checkNodeCount(1)

      def assignment = nestedBlock.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def identifierX = assignment.expandAst(NodeTypes.IDENTIFIER)
      identifierX.checkNodeCount(1)

      def localXViaRef = identifierX.expandRef(NodeTypes.LOCAL)
      localXViaRef.head shouldBe localX.head
    }

    "have correct references for shadowing let" in AstFixture("""
          | let x
          | {
          |   let x
          |   x = 1
          | }
          | x = 1
        """.stripMargin) { cpg =>
      def method = cpg.method.nameExact(":program")
      method.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def outerLocalX = methodBlock.expandAst(NodeTypes.LOCAL)
      outerLocalX.checkNodeCount(1)

      def nestedBlock = methodBlock.expandAst(NodeTypes.BLOCK)
      nestedBlock.checkNodeCount(1)

      def innerLocalX = nestedBlock.expandAst(NodeTypes.LOCAL)
      innerLocalX.checkNodeCount(1)

      def innerAssignment = nestedBlock.expandAst(NodeTypes.CALL)
      innerAssignment.checkNodeCount(1)

      def innerIdentifierX = innerAssignment.expandAst(NodeTypes.IDENTIFIER)
      innerIdentifierX.checkNodeCount(1)

      def innerLocalXViaRef = innerIdentifierX.expandRef(NodeTypes.LOCAL)
      innerLocalXViaRef.head shouldBe innerLocalX.head

      def outerAssignment = methodBlock.expandAst(NodeTypes.CALL)
      outerAssignment.checkNodeCount(1)

      def outerIdentifierX = outerAssignment.expandAst(NodeTypes.IDENTIFIER)
      outerIdentifierX.checkNodeCount(1)

      def outerLocalXViaRef = outerIdentifierX.expandRef(NodeTypes.LOCAL)
      outerLocalXViaRef.head shouldBe outerLocalX.head
    }

    "have correct closure binding (single variable)" in AstFixture("""
         | function foo()
         | {
         |   x = 1
         |   function bar() {
         |     x = 2
         |   }
         | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBinding = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBinding.checkNodeCount(1)
      closureBinding.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")
      closureBinding.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBinding.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBinding.expandRef().head shouldBe fooLocalX.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocals = barMethodBlock.expandAst(NodeTypes.LOCAL)
      barLocals.checkNodeCount(1)
      barLocals.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")

      def identifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      identifierX.checkNodeCount(1)

      identifierX.expandRef().head shouldBe barLocals.head
    }

    "have correct closure binding (two variables)" in AstFixture("""
         | function foo()
         | {
         |   x = 1
         |   y = 1
         |   function bar() {
         |     x = 2
         |     y = 2
         |   }
         | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def fooLocalY = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "y")
      fooLocalY.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBinding = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBinding.checkNodeCount(2)

      def closureBindForX = closureBinding.filter(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindForX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")
      closureBindForX.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindForX.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindForX.expandRef().head shouldBe fooLocalX.head

      def closureBindForY = closureBinding.filter(PropertyNames.CLOSURE_ORIGINAL_NAME, "y")
      closureBindForY.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:y")
      closureBindForY.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "y")
      closureBindForY.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindForY.expandRef().head shouldBe fooLocalY.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocals = barMethodBlock.expandAst(NodeTypes.LOCAL)
      barLocals.checkNodeCount(2)

      def barLocalsForX = barLocals.filter(PropertyNames.NAME, "x")
      barLocalsForX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")

      def identifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      identifierX.checkNodeCount(1)

      identifierX.expandRef().head shouldBe barLocalsForX.head

      def barLocalsForY = barLocals.filter(PropertyNames.NAME, "y")
      barLocalsForY.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:y")

      def identifierY =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "y")
      identifierY.checkNodeCount(1)

      identifierY.expandRef().head shouldBe barLocalsForY.head
    }

    "have correct closure binding for capturing over 2 levels" in AstFixture("""
         | function foo()
         | {
         |   x = 1
         |   function bar() {
         |     x = 2
         |     function baz() {
         |       x = 3
         |     }
         |   }
         | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBindingXInFoo = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInFoo.checkNodeCount(1)
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInFoo.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInFoo.expandRef().head shouldBe fooLocalX.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocalX = barMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      barLocalX.checkNodeCount(1)
      barLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")

      def barIdentifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      barIdentifierX.checkNodeCount(1)

      barIdentifierX.expandRef().head shouldBe barLocalX.head

      def bazRef = barMethodBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      bazRef.checkNodeCount(1)

      def closureBindingXInBar = bazRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInBar.checkNodeCount(1)
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInBar.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInBar.expandRef().head shouldBe barLocalX.head

      def bazMethod = cpg.method.nameExact("baz")
      bazMethod.checkNodeCount(1)

      def bazMethodBlock = bazMethod.expandAst(NodeTypes.BLOCK)
      bazMethodBlock.checkNodeCount(1)

      def bazLocalX = bazMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      bazLocalX.checkNodeCount(1)
      bazLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")

      def bazIdentifierX =
        bazMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      bazIdentifierX.checkNodeCount(1)

      bazIdentifierX.expandRef().head shouldBe bazLocalX.head
    }

    "have correct closure binding for capturing over 2 levels with intermediate blocks" in AstFixture("""
          | function foo()
          | {
          |   x = 1
          |   function bar() {
          |     x = 2
          |     {
          |       function baz() {
          |         {
          |            x = 3
          |         }
          |       }
          |     }
          |   }
          | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBindingXInFoo = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInFoo.checkNodeCount(1)
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInFoo.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInFoo.expandRef().head shouldBe fooLocalX.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocalX = barMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      barLocalX.checkNodeCount(1)
      barLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")

      def barIdentifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      barIdentifierX.checkNodeCount(1)

      barIdentifierX.expandRef().head shouldBe barLocalX.head

      def barMethodInnerBlock = barMethodBlock.expandAst(NodeTypes.BLOCK)
      barMethodInnerBlock.checkNodeCount(1)

      def bazRef = barMethodInnerBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      bazRef.checkNodeCount(1)

      def closureBindingXInBar = bazRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInBar.checkNodeCount(1)
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInBar.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInBar.expandRef().head shouldBe barLocalX.head

      def bazMethod = cpg.method.nameExact("baz")
      bazMethod.checkNodeCount(1)

      def bazMethodBlock = bazMethod.expandAst(NodeTypes.BLOCK)
      bazMethodBlock.checkNodeCount(1)

      def bazLocalX = bazMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      bazLocalX.checkNodeCount(1)
      bazLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")

      def bazMethodInnerBlock = bazMethodBlock.expandAst(NodeTypes.BLOCK)
      bazMethodInnerBlock.checkNodeCount(1)

      def bazIdentifierX =
        bazMethodInnerBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      bazIdentifierX.checkNodeCount(1)

      bazIdentifierX.expandRef().head shouldBe bazLocalX.head
    }

    "have correct closure binding for capturing over 2 levels with no intermediate use" in AstFixture("""
          | function foo()
          | {
          |   x = 1
          |   function bar() {
          |     function baz() {
          |       x = 3
          |     }
          |   }
          | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBindingXInFoo = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInFoo.checkNodeCount(1)
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")
      closureBindingXInFoo.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInFoo.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInFoo.expandRef().head shouldBe fooLocalX.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocalX = barMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      barLocalX.checkNodeCount(1)
      barLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:x")

      def bazRef = barMethodBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      bazRef.checkNodeCount(1)

      def closureBindingXInBar = bazRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXInBar.checkNodeCount(1)
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")
      closureBindingXInBar.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXInBar.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXInBar.expandRef().head shouldBe barLocalX.head

      def bazMethod = cpg.method.nameExact("baz")
      bazMethod.checkNodeCount(1)

      def bazMethodBlock = bazMethod.expandAst(NodeTypes.BLOCK)
      bazMethodBlock.checkNodeCount(1)

      def bazLocalX = bazMethodBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      bazLocalX.checkNodeCount(1)
      bazLocalX.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:bar:baz:x")

      def bazIdentifierX =
        bazMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      bazIdentifierX.checkNodeCount(1)

      bazIdentifierX.expandRef().head shouldBe bazLocalX.head
    }

    "have correct closure binding for capturing the same variable into 2 different anonymous methods" in AstFixture("""
          | function foo()
          | {
          |   var x = 1
          |   var anon1 = y => 2*x
          |   var anon2 = y => 2*x
          | }
        """.stripMargin) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def anon1Ref =
        fooBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.METHOD_REF)
          .filter(PropertyNames.METHOD_FULL_NAME, "code.js::program:foo:anonymous")
      anon1Ref.checkNodeCount(1)

      def closureBindingXAnon1 = anon1Ref.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXAnon1.checkNodeCount(1)
      closureBindingXAnon1.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:anonymous:x")
      closureBindingXAnon1.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXAnon1.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXAnon1.expandRef().head shouldBe fooLocalX.head

      def anon2Ref =
        fooBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.METHOD_REF)
          .filter(PropertyNames.METHOD_FULL_NAME, "code.js::program:foo:anonymous1")
      anon2Ref.checkNodeCount(1)

      def closureBindingXAnon2 = anon2Ref.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBindingXAnon2.checkNodeCount(1)
      closureBindingXAnon2.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "code.js::program:foo:anonymous1:x")
      closureBindingXAnon2.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBindingXAnon2.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBindingXAnon2.expandRef().head shouldBe fooLocalX.head
    }

    "have correct closure binding when using an external source file" in AstFixture(
      File(getClass.getResource("/closurebinding/foobar.js").toURI)
    ) { cpg =>
      def fooMethod = cpg.method.nameExact("foo")
      fooMethod.checkNodeCount(1)

      def fooBlock = fooMethod.expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def fooLocalX = fooBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      fooLocalX.checkNodeCount(1)

      def barRef = fooBlock.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      barRef.checkNodeCount(1)

      def closureBinding = barRef.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBinding.checkNodeCount(1)
      closureBinding.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "foobar.js::program:foo:bar:x")
      closureBinding.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBinding.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBinding.expandRef().head shouldBe fooLocalX.head

      def barMethod = cpg.method.nameExact("bar")
      barMethod.checkNodeCount(1)

      def barMethodBlock = barMethod.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocals = barMethodBlock.expandAst(NodeTypes.LOCAL)
      barLocals.checkNodeCount(1)
      barLocals.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "foobar.js::program:foo:bar:x")

      def identifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      identifierX.checkNodeCount(1)

      identifierX.expandRef().head shouldBe barLocals.head
    }

    "have correct closure binding when using an external nested source file" in AstFixture(
      File(getClass.getResource("/closurebinding/nested/a.js").toURI)
    ) { cpg =>
      def a1Method = cpg.method.nameExact("a1")
      a1Method.checkNodeCount(1)

      def a1Block = a1Method.expandAst(NodeTypes.BLOCK)
      a1Block.checkNodeCount(1)

      def a1LocalX = a1Block.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "x")
      a1LocalX.checkNodeCount(1)

      def a1Ref = a1Block.expandAst(NodeTypes.CALL).expandAst(NodeTypes.METHOD_REF)
      a1Ref.checkNodeCount(1)

      def closureBinding = a1Ref.expandCapture(NodeTypes.CLOSURE_BINDING)
      closureBinding.checkNodeCount(1)
      closureBinding.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "a.js::program:a1:a2:x")
      closureBinding.checkProperty(PropertyNames.CLOSURE_ORIGINAL_NAME, "x")
      closureBinding.checkProperty(PropertyNames.EVALUATION_STRATEGY, EvaluationStrategies.BY_REFERENCE)

      closureBinding.expandRef().head shouldBe a1LocalX.head

      def method = cpg.method.nameExact("a2")
      method.checkNodeCount(1)

      def barMethodBlock = method.expandAst(NodeTypes.BLOCK)
      barMethodBlock.checkNodeCount(1)

      def barLocals = barMethodBlock.expandAst(NodeTypes.LOCAL)
      barLocals.checkNodeCount(1)
      barLocals.checkProperty(PropertyNames.CLOSURE_BINDING_ID, "a.js::program:a1:a2:x")

      def identifierX =
        barMethodBlock
          .expandAst(NodeTypes.CALL)
          .expandAst(NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "x")
      identifierX.checkNodeCount(1)

      identifierX.expandRef().head shouldBe barLocals.head
    }

    "have correct method full names for scoped anonymous functions" in AstFixture("""
        | var anon1 = x => {
        |   var anon2 = y => {
        |   }
        | }
        | var anon3 = x => {
        |   var anon4 = y => {
        |   }
        | }
        |        """.stripMargin) { cpg =>
      cpg.method.lineNumber(2).head.fullName should be("code.js::program:anonymous")
      cpg.method.lineNumber(3).head.fullName should be("code.js::program:anonymous:anonymous")
      cpg.method.lineNumber(6).head.fullName should be("code.js::program:anonymous1")
      cpg.method.lineNumber(7).head.fullName should be("code.js::program:anonymous1:anonymous")
    }
  }

  "AST generation for mixed fragments" should {
    "simple js fragment with call" in AstFixture("""
         |function source(a) { return a; }
         |var l = source(3)
        """.stripMargin) { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def method = cpg.method.nameExact("source")
      method.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def methodBlock = method.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def methodReturn = method.expandAst(NodeTypes.METHOD_RETURN)
      methodReturn.checkNodeCount(1)

      def programReturn = program.expandAst(NodeTypes.METHOD_RETURN)
      programReturn.checkNodeCount(1)

      def methodParamIn = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      methodParamIn.checkNodeCount(2)

      def locals = programBlock.expandAst(NodeTypes.LOCAL)
      locals.checkNodeCount(2)

      def localSource = locals.filter(PropertyNames.NAME, "source")
      localSource.checkNodeCount(1)
      localSource.checkProperty(PropertyNames.TYPE_FULL_NAME, "code.js::program:source")

      def localL = locals.filter(PropertyNames.NAME, "l")
      localL.checkNodeCount(1)

      def callToSource =
        programBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "l = source(3)")
      callToSource.checkNodeCount(1)

      def identifierL = callToSource.expandAst(NodeTypes.IDENTIFIER)
      identifierL.checkNodeCount(1)
      identifierL.checkProperty(PropertyNames.NAME, "l")

      def call = callToSource.expandAst(NodeTypes.CALL)
      call.checkNodeCount(1)

      def literal = call.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "3")
      literal.checkNodeCount(1)

      def returnFromMethod = methodBlock.expandAst(NodeTypes.RETURN)
      returnFromMethod.checkNodeCount(1)

      def methodReturnIdent =
        returnFromMethod.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      methodReturnIdent.checkNodeCount(1)
    }

    "simple js fragment with array access" in AstFixture("result = rows[0].solution;") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def call = programBlock.expandAst(NodeTypes.CALL)
      call.checkNodeCount(1)

      def rowsCall = call.expandAst(NodeTypes.CALL)
      rowsCall.checkNodeCount(1)

      def solutionIdentifier =
        rowsCall
          .expandAst(NodeTypes.FIELD_IDENTIFIER)
          .filter(PropertyNames.CANONICAL_NAME, "solution")
      solutionIdentifier.checkNodeCount(1)

      def rowsCallLeft = rowsCall.expandAst(NodeTypes.CALL)
      rowsCallLeft.checkNodeCount(1)

      def literal = rowsCallLeft.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "0")
      literal.checkNodeCount(1)

      def rowsIdentifier =
        rowsCallLeft.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "rows")
      rowsIdentifier.checkNodeCount(1)

      def resultIdentifier =
        call.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "result")
      resultIdentifier.checkNodeCount(1)
    }

  }

  "AST generation for destructing assignment" should {
    "have correct structure for object destruction assignment with declaration" in AstFixture("var {a, b} = x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
      localB.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0.a")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def fieldAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
      fieldAccessA.checkNodeCount(1)
      fieldAccessA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def leftA = fieldAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA =
        fieldAccessA.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, "a")
      rightA.checkNodeCount(1)

      def assignmentToB =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0.b")
      assignmentToB.checkNodeCount(1)
      def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def fieldAccessB =
        assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
      fieldAccessB.checkNodeCount(1)
      fieldAccessB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def leftB = fieldAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftB.checkNodeCount(1)
      def rightB =
        fieldAccessB.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, "b")
      rightB.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with declaration and ternary init" in AstFixture(
      "const { a, b } = test() ? foo() : bar();"
    ) { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
      localB.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_0 = test() ? foo() : bar()")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0.a")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def fieldAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
      fieldAccessA.checkNodeCount(1)
      fieldAccessA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def leftA = fieldAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA =
        fieldAccessA.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, "a")
      rightA.checkNodeCount(1)

      def assignmentToB =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0.b")
      assignmentToB.checkNodeCount(1)
      def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def fieldAccessB =
        assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
      fieldAccessB.checkNodeCount(1)
      fieldAccessB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def leftB = fieldAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftB.checkNodeCount(1)
      def rightB =
        fieldAccessB.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, "b")
      rightB.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment without declaration" in AstFixture("({a, b} = x)") {
      cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
        localA.checkNodeCount(1)

        def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
        localB.checkNodeCount(1)

        def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
        destructionBlock.checkNodeCount(1)

        def localTmp =
          destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
        localTmp.checkNodeCount(1)

        def assignmentToTmp =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
        assignmentToTmp.checkNodeCount(1)

        def assignmentToA =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0.a")
        assignmentToA.checkNodeCount(1)
        def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
        a.checkNodeCount(1)

        def fieldAccessA =
          assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
        fieldAccessA.checkNodeCount(1)
        fieldAccessA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftA = fieldAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftA.checkNodeCount(1)
        def rightA =
          fieldAccessA
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "a")
        rightA.checkNodeCount(1)

        def assignmentToB =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0.b")
        assignmentToB.checkNodeCount(1)
        def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
        b.checkNodeCount(1)

        def fieldAccessB =
          assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
        fieldAccessB.checkNodeCount(1)
        fieldAccessB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftB = fieldAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftB.checkNodeCount(1)
        def rightB =
          fieldAccessB
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "b")
        rightB.checkNodeCount(1)

        def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
        tmpReturnIdentifier.checkNodeCount(1)
        tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with defaults" in AstFixture("var {a = 1, b = 2} = x") {
      cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
        localA.checkNodeCount(1)

        def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
        localB.checkNodeCount(1)

        def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
        destructionBlock.checkNodeCount(1)

        def localTmp =
          destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
        localTmp.checkNodeCount(1)

        def assignmentToTmp =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
        assignmentToTmp.checkNodeCount(1)

        def assignmentToA =
          destructionBlock
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.NAME, Operators.assignment)
            .filter(PropertyNames.CODE, "a = _tmp_0.a === void 0 ? 1 : _tmp_0.a")
        assignmentToA.checkNodeCount(1)

        def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
        a.checkNodeCount(1)

        def ifA =
          assignmentToA
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.CODE, "_tmp_0.a === void 0 ? 1 : _tmp_0.a")
        ifA.checkNodeCount(1)
        ifA.checkProperty(PropertyNames.NAME, Operators.conditional)

        def testA = ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a === void 0")
        testA.checkNodeCount(1)
        testA.checkProperty(PropertyNames.NAME, Operators.equals)

        def testAFieldAccess =
          testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
        testAFieldAccess.checkNodeCount(1)
        testAFieldAccess.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def voidCallA = testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
        voidCallA.checkNodeCount(1)

        def trueBranchA = ifA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        trueBranchA.checkNodeCount(1)

        def falseBranchA =
          ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
        falseBranchA.checkNodeCount(1)
        falseBranchA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def assignmentToB =
          destructionBlock
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.NAME, Operators.assignment)
            .filter(PropertyNames.CODE, "b = _tmp_0.b === void 0 ? 2 : _tmp_0.b")
        assignmentToB.checkNodeCount(1)

        def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
        b.checkNodeCount(1)

        def ifB =
          assignmentToB
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.CODE, "_tmp_0.b === void 0 ? 2 : _tmp_0.b")
        ifB.checkNodeCount(1)
        ifB.checkProperty(PropertyNames.NAME, Operators.conditional)

        def testB = ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b === void 0")
        testB.checkNodeCount(1)
        testB.checkProperty(PropertyNames.NAME, Operators.equals)

        def testBFieldAccess =
          testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
        testBFieldAccess.checkNodeCount(1)
        testBFieldAccess.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def voidCallB = testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
        voidCallB.checkNodeCount(1)

        def trueBranchB = ifB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
        trueBranchB.checkNodeCount(1)

        def falseBranchB =
          ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
        falseBranchB.checkNodeCount(1)
        falseBranchB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
        tmpReturnIdentifier.checkNodeCount(1)
        tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with reassignment" in AstFixture("var {a: n, b: m} = x") {
      cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def localN = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "n")
        localN.checkNodeCount(1)

        def localM = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "m")
        localM.checkNodeCount(1)

        def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
        destructionBlock.checkNodeCount(1)

        def localTmp =
          destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
        localTmp.checkNodeCount(1)

        def assignmentToTmp =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
        assignmentToTmp.checkNodeCount(1)

        def assignmentToN =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "n = _tmp_0.a")
        assignmentToN.checkNodeCount(1)

        def n = assignmentToN.expandAst(NodeTypes.IDENTIFIER)
        n.checkNodeCount(1)

        def fieldAccessN =
          assignmentToN.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
        fieldAccessN.checkNodeCount(1)
        fieldAccessN.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftN = fieldAccessN.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftN.checkNodeCount(1)

        def rightN =
          fieldAccessN
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "a")
        rightN.checkNodeCount(1)

        def assignmentToM =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "m = _tmp_0.b")
        assignmentToM.checkNodeCount(1)

        def m = assignmentToM.expandAst(NodeTypes.IDENTIFIER)
        m.checkNodeCount(1)

        def fieldAccessM =
          assignmentToM.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
        fieldAccessM.checkNodeCount(1)
        fieldAccessM.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftM = fieldAccessM.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftM.checkNodeCount(1)

        def rightM =
          fieldAccessM
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "b")
        rightM.checkNodeCount(1)

        def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
        tmpReturnIdentifier.checkNodeCount(1)
        tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with reassignment and defaults" in AstFixture(
      "var {a: n = 1, b: m = 2} = x"
    ) { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localN = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "n")
      localN.checkNodeCount(1)

      def localM = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "m")
      localM.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToN =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.NAME, Operators.assignment)
          .filter(PropertyNames.CODE, "n = _tmp_0.a === void 0 ? 1 : _tmp_0.a")
      assignmentToN.checkNodeCount(1)

      def a = assignmentToN.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def ifA =
        assignmentToN
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_0.a === void 0 ? 1 : _tmp_0.a")
      ifA.checkNodeCount(1)
      ifA.checkProperty(PropertyNames.NAME, Operators.conditional)

      def testA = ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a === void 0")
      testA.checkNodeCount(1)
      testA.checkProperty(PropertyNames.NAME, Operators.equals)

      def testAFieldAccess =
        testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
      testAFieldAccess.checkNodeCount(1)
      testAFieldAccess.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def voidCallA = testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
      voidCallA.checkNodeCount(1)

      def trueBranchA = ifA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      trueBranchA.checkNodeCount(1)

      def falseBranchA =
        ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
      falseBranchA.checkNodeCount(1)
      falseBranchA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def assignmentToM =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.NAME, Operators.assignment)
          .filter(PropertyNames.CODE, "m = _tmp_0.b === void 0 ? 2 : _tmp_0.b")
      assignmentToM.checkNodeCount(1)

      def b = assignmentToN.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def ifB =
        assignmentToM
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_0.b === void 0 ? 2 : _tmp_0.b")
      ifB.checkNodeCount(1)
      ifB.checkProperty(PropertyNames.NAME, Operators.conditional)

      def testB = ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b === void 0")
      testB.checkNodeCount(1)
      testB.checkProperty(PropertyNames.NAME, Operators.equals)

      def testBFieldAccess =
        testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
      testBFieldAccess.checkNodeCount(1)
      testBFieldAccess.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def voidCallB = testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
      voidCallB.checkNodeCount(1)

      def trueBranchB = ifB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
      trueBranchB.checkNodeCount(1)

      def falseBranchB =
        ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
      falseBranchB.checkNodeCount(1)
      falseBranchB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object deconstruction in function parameter" in AstFixture(
      "function foo({ a }, b) {}"
    ) { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def fooMethod = program.expandAst(NodeTypes.METHOD).filter(PropertyNames.NAME, "foo")
      fooMethod.checkNodeCount(1)

      def a =
        fooMethod.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "param1_0")
      a.checkNodeCount(1)
      a.checkProperty(PropertyNames.CODE, "{ a }")
      a.checkProperty(PropertyNames.INDEX, 1)

      def b = fooMethod.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "b")
      b.checkNodeCount(1)
      b.checkProperty(PropertyNames.CODE, "b")
      b.checkProperty(PropertyNames.INDEX, 2)
    }

    "have correct structure for object destruction assignment in call argument" in AstFixture("foo({a, b} = x)") {
      cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
        localA.checkNodeCount(1)

        def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
        localB.checkNodeCount(1)

        def fooCall = programBlock.expandAst(NodeTypes.CALL)
        fooCall.checkNodeCount(1)

        def destructionBlock = fooCall.expandAst(NodeTypes.BLOCK)
        destructionBlock.checkNodeCount(1)

        def localTmp =
          destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
        localTmp.checkNodeCount(1)

        def assignmentToTmp =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
        assignmentToTmp.checkNodeCount(1)

        def assignmentToA =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0.a")
        assignmentToA.checkNodeCount(1)
        def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
        a.checkNodeCount(1)

        def fieldAccessA =
          assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
        fieldAccessA.checkNodeCount(1)
        fieldAccessA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftA = fieldAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftA.checkNodeCount(1)
        def rightA =
          fieldAccessA
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "a")
        rightA.checkNodeCount(1)

        def assignmentToB =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0.b")
        assignmentToB.checkNodeCount(1)
        def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
        b.checkNodeCount(1)

        def fieldAccessB =
          assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.b")
        fieldAccessB.checkNodeCount(1)
        fieldAccessB.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

        def leftB = fieldAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
        leftB.checkNodeCount(1)
        def rightB =
          fieldAccessB
            .expandAst(NodeTypes.FIELD_IDENTIFIER)
            .filter(PropertyNames.CANONICAL_NAME, "b")
        rightB.checkNodeCount(1)

        def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
        tmpReturnIdentifier.checkNodeCount(1)
        tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with rest" in AstFixture("var {a, ...rest} = x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0.a")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def fieldAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0.a")
      fieldAccessA.checkNodeCount(1)
      fieldAccessA.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

      def leftA = fieldAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA =
        fieldAccessA.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, "a")
      rightA.checkNodeCount(1)

      def unknownRest =
        destructionBlock.expandAst(NodeTypes.UNKNOWN).filter(PropertyNames.CODE, "...rest")
      unknownRest.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for object destruction assignment with computed property name" ignore AstFixture(
      "var {[propName]: n} = x"
    ) { _ => }

    "have correct structure for nested object destruction assignment with defaults as parameter" in AstFixture("""
        |function userId({id = {}, b} = {}) {
        |  return id
        |}
        |""".stripMargin) { cpg =>
      def userId = cpg.method.nameExact("userId")
      userId.checkNodeCount(1)

      def param =
        userId.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "param1_0")
      param.checkNodeCount(1)
      param.checkProperty(PropertyNames.CODE, "{id = {}, b} = {}")

      def userIdBlock = userId.expandAst(NodeTypes.BLOCK)
      userIdBlock.checkNodeCount(1)

      def destructionBlock = userIdBlock.expandAst(NodeTypes.BLOCK).filter(PropertyNames.ORDER, 1)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_1")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_1 = param1_0 === void 0 ? {} : param1_0")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToId =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "id = _tmp_1.id === void 0 ? {} : _tmp_1.id")
      assignmentToId.checkNodeCount(1)

      def assignmentToB =
        destructionBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "b = _tmp_1.b")
      assignmentToB.checkNodeCount(1)

      def id = assignmentToId.expandAst(NodeTypes.IDENTIFIER)
      id.checkNodeCount(1)

      def ternaryId =
        assignmentToId
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "_tmp_1.id === void 0 ? {} : _tmp_1.id")
      ternaryId.checkNodeCount(1)

      def indexAccessId =
        ternaryId.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_1.id")
      indexAccessId.checkNodeCount(1)

      def leftId =
        indexAccessId.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_1")
      leftId.checkNodeCount(1)
      def rightId =
        indexAccessId
          .expandAst(NodeTypes.FIELD_IDENTIFIER)
          .filter(PropertyNames.CANONICAL_NAME, "id")
      rightId.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_1")
    }

    "have correct structure for object destruction assignment as parameter" in AstFixture("""
        |function userId({id}) {
        |  return id
        |}
        |""".stripMargin) { cpg =>
      def userId = cpg.method.nameExact("userId")
      userId.checkNodeCount(1)

      def userIdBlock = userId.expandAst(NodeTypes.BLOCK)
      userIdBlock.checkNodeCount(1)

      def idLocal = userIdBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "id")
      idLocal.checkNodeCount(1)

      def assignmentToId =
        userIdBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "id = param1_0.id")
      assignmentToId.checkNodeCount(1)
      def id = assignmentToId.expandAst(NodeTypes.IDENTIFIER)
      id.checkNodeCount(1)

      def indexAccessId =
        assignmentToId.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "param1_0.id")
      indexAccessId.checkNodeCount(1)

      def leftId =
        indexAccessId.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "param1_0")
      leftId.checkNodeCount(1)
      def rightId =
        indexAccessId
          .expandAst(NodeTypes.FIELD_IDENTIFIER)
          .filter(PropertyNames.CANONICAL_NAME, "id")
      rightId.checkNodeCount(1)
    }

    "have correct structure for array destruction assignment with declaration" in AstFixture("var [a, b] = x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
      localB.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0[0]")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def indexAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0]")
      indexAccessA.checkNodeCount(1)
      indexAccessA.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftA =
        indexAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA = indexAccessA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "0")
      rightA.checkNodeCount(1)

      def assignmentToB =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0[1]")
      assignmentToB.checkNodeCount(1)
      def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def indexAccessB =
        assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[1]")
      indexAccessB.checkNodeCount(1)
      indexAccessB.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftB =
        indexAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftB.checkNodeCount(1)
      def rightB = indexAccessB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      rightB.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for array destruction assignment without declaration" in AstFixture("[a, b] = x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
      localB.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0[0]")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def indexAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0]")
      indexAccessA.checkNodeCount(1)
      indexAccessA.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftA =
        indexAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA = indexAccessA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "0")
      rightA.checkNodeCount(1)

      def assignmentToB =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0[1]")
      assignmentToB.checkNodeCount(1)
      def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def indexAccessB =
        assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[1]")
      indexAccessB.checkNodeCount(1)
      indexAccessB.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftB =
        indexAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftB.checkNodeCount(1)
      def rightB = indexAccessB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      rightB.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for array destruction assignment with defaults" in AstFixture("var [a = 1, b = 2] = x") {
      cpg =>
        def program = cpg.method.nameExact(":program")
        program.checkNodeCount(1)

        def programBlock = program.expandAst(NodeTypes.BLOCK)
        programBlock.checkNodeCount(1)

        def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
        localA.checkNodeCount(1)

        def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
        localB.checkNodeCount(1)

        def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
        destructionBlock.checkNodeCount(1)

        def localTmp =
          destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
        localTmp.checkNodeCount(1)

        def assignmentToTmp =
          destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
        assignmentToTmp.checkNodeCount(1)

        def assignmentToA =
          destructionBlock
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.NAME, Operators.assignment)
            .filter(PropertyNames.CODE, "a = _tmp_0[0] === void 0 ? 1 : _tmp_0[0]")
        assignmentToA.checkNodeCount(1)

        def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
        a.checkNodeCount(1)

        def ifA =
          assignmentToA
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.CODE, "_tmp_0[0] === void 0 ? 1 : _tmp_0[0]")
        ifA.checkNodeCount(1)
        ifA.checkProperty(PropertyNames.NAME, Operators.conditional)

        def testA = ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0] === void 0")
        testA.checkNodeCount(1)
        testA.checkProperty(PropertyNames.NAME, Operators.equals)

        def testAIndexAccess =
          testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0]")
        testAIndexAccess.checkNodeCount(1)
        testAIndexAccess.checkProperty(PropertyNames.NAME, Operators.indexAccess)

        def voidCallA = testA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
        voidCallA.checkNodeCount(1)

        def trueBranchA = ifA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        trueBranchA.checkNodeCount(1)

        def falseBranchA =
          ifA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0]")
        falseBranchA.checkNodeCount(1)
        falseBranchA.checkProperty(PropertyNames.NAME, Operators.indexAccess)

        def assignmentToB =
          destructionBlock
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.NAME, Operators.assignment)
            .filter(PropertyNames.CODE, "b = _tmp_0[1] === void 0 ? 2 : _tmp_0[1]")
        assignmentToB.checkNodeCount(1)

        def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
        b.checkNodeCount(1)

        def ifB =
          assignmentToB
            .expandAst(NodeTypes.CALL)
            .filter(PropertyNames.CODE, "_tmp_0[1] === void 0 ? 2 : _tmp_0[1]")
        ifB.checkNodeCount(1)
        ifB.checkProperty(PropertyNames.NAME, Operators.conditional)

        def testB = ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[1] === void 0")
        testB.checkNodeCount(1)
        testB.checkProperty(PropertyNames.NAME, Operators.equals)

        def testBIndexAccess =
          testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[1]")
        testBIndexAccess.checkNodeCount(1)
        testBIndexAccess.checkProperty(PropertyNames.NAME, Operators.indexAccess)

        def voidCallB = testB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "void 0")
        voidCallB.checkNodeCount(1)

        def trueBranchB = ifB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
        trueBranchB.checkNodeCount(1)

        def falseBranchB =
          ifB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[1]")
        falseBranchB.checkNodeCount(1)
        falseBranchB.checkProperty(PropertyNames.NAME, Operators.indexAccess)

        def returnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
        returnIdentifier.checkNodeCount(1)
        returnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for array destruction assignment with ignores" in AstFixture("var [a, , b] = x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def localA = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "a")
      localA.checkNodeCount(1)

      def localB = programBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "b")
      localB.checkNodeCount(1)

      def destructionBlock = programBlock.expandAst(NodeTypes.BLOCK)
      destructionBlock.checkNodeCount(1)

      def localTmp =
        destructionBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
      localTmp.checkNodeCount(1)

      def assignmentToTmp =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0 = x")
      assignmentToTmp.checkNodeCount(1)

      def assignmentToA =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = _tmp_0[0]")
      assignmentToA.checkNodeCount(1)
      def a = assignmentToA.expandAst(NodeTypes.IDENTIFIER)
      a.checkNodeCount(1)

      def indexAccessA =
        assignmentToA.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[0]")
      indexAccessA.checkNodeCount(1)
      indexAccessA.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftA =
        indexAccessA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftA.checkNodeCount(1)
      def rightA = indexAccessA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "0")
      rightA.checkNodeCount(1)

      def assignmentToB =
        destructionBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = _tmp_0[2]")
      assignmentToB.checkNodeCount(1)
      def b = assignmentToB.expandAst(NodeTypes.IDENTIFIER)
      b.checkNodeCount(1)

      def indexAccessB =
        assignmentToB.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0[2]")
      indexAccessB.checkNodeCount(1)
      indexAccessB.checkProperty(PropertyNames.NAME, Operators.indexAccess)

      def leftB =
        indexAccessB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
      leftB.checkNodeCount(1)
      def rightB = indexAccessB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
      rightB.checkNodeCount(1)

      def tmpReturnIdentifier = destructionBlock.expandAst(NodeTypes.IDENTIFIER)
      tmpReturnIdentifier.checkNodeCount(1)
      tmpReturnIdentifier.checkProperty(PropertyNames.NAME, "_tmp_0")
    }

    "have correct structure for array destruction assignment with rest" ignore AstFixture("var [a, ...rest] = x") { _ =>
    }

    "have correct structure for array destruction as parameter" in AstFixture("""
       |function userId([id]) {
       |  return id
       |}
       |""".stripMargin) { cpg =>
      def userId = cpg.method.nameExact("userId")
      userId.checkNodeCount(1)

      def param =
        userId.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "param1_0")
      param.checkNodeCount(1)

      def userIdBlock = userId.expandAst(NodeTypes.BLOCK)
      userIdBlock.checkNodeCount(1)

      def idLocal = userIdBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "id")
      idLocal.checkNodeCount(1)

      def assignmentToId =
        userIdBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "id = param1_0.id")
      assignmentToId.checkNodeCount(1)
    }

    "have correct structure for method spread argument" ignore AstFixture("foo(...args)") { _ => }
  }

  "AST generation for classes" should {
    "have a TYPE_DECL and <meta> TYPE_DECL for ClassA" in AstFixture("var x = class ClassA {}") { cpg =>
      def classAMetaTypeDecl =
        cpg.typeDecl
          .nameExact("ClassA<meta>")
          .filter(PropertyNames.FULL_NAME, "code.js::program:ClassA<meta>")
      classAMetaTypeDecl.checkNodeCount(1)

      def classATypeDecl =
        cpg.typeDecl
          .nameExact("ClassA")
          .filter(PropertyNames.FULL_NAME, "code.js::program:ClassA")
      classATypeDecl.checkNodeCount(1)
    }

    "have constructor binding in <meta> TYPE_DECL for ClassA" in AstFixture("""
        |var x = class ClassA {
        |  constructor() {}
        |}""".stripMargin) { cpg =>
      def classAMetaTypeDecl =
        cpg.typeDecl
          .nameExact("ClassA<meta>")
          .filter(PropertyNames.FULL_NAME, "code.js::program:ClassA<meta>")
      classAMetaTypeDecl.checkNodeCount(1)

      def constructorBinding = classAMetaTypeDecl.expand(EdgeTypes.BINDS, NodeTypes.BINDING)
      constructorBinding.checkNodeCount(1)
      constructorBinding.checkProperty(PropertyNames.NAME, "")
      constructorBinding.checkProperty(PropertyNames.SIGNATURE, "")

      def boundMethod = constructorBinding.expandRef(NodeTypes.METHOD)
      boundMethod.checkNodeCount(1)
      boundMethod.checkProperty(PropertyNames.FULL_NAME, "code.js::program:ClassA<constructor>")
      boundMethod.checkProperty(PropertyNames.CODE, "constructor() {}")
    }

    "have member for static method in <meta> TYPE_DECL for ClassA" in AstFixture("""
       |var x = class ClassA {
       |  static staticFoo() {}
       |}""".stripMargin) { cpg =>
      def classAMetaTypeDecl =
        cpg.typeDecl
          .nameExact("ClassA<meta>")
          .filter(PropertyNames.FULL_NAME, "code.js::program:ClassA<meta>")
      classAMetaTypeDecl.checkNodeCount(1)

      def memberFoo = classAMetaTypeDecl.expandAst(NodeTypes.MEMBER)
      memberFoo.checkNodeCount(1)
      memberFoo.checkProperty(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq("code.js::program:ClassA:staticFoo"))
      memberFoo.checkProperty(PropertyNames.CODE, "static staticFoo() {}")
    }

    "have method for static method in ClassA AST" in AstFixture("""
        |var x = class ClassA {
        |  static staticFoo() {}
        |}""".stripMargin) { cpg =>
      def classATypeDecl =
        cpg.typeDecl.nameExact("ClassA").filter(PropertyNames.FULL_NAME, "code.js::program:ClassA")
      classATypeDecl.checkNodeCount(1)

      def methodFoo =
        classATypeDecl.expandAst(NodeTypes.METHOD).filter(PropertyNames.NAME, "staticFoo")
      methodFoo.checkNodeCount(1)
      methodFoo.checkProperty(PropertyNames.FULL_NAME, "code.js::program:ClassA:staticFoo")
      methodFoo.checkProperty(PropertyNames.CODE, "static staticFoo() {}")
    }

    "have member for non-static method in TYPE_DECL for ClassA" in AstFixture("""
        |var x = class ClassA {
        |  foo() {}
        |}""".stripMargin) { cpg =>
      def classATypeDecl =
        cpg.typeDecl.nameExact("ClassA").filter(PropertyNames.FULL_NAME, "code.js::program:ClassA")
      classATypeDecl.checkNodeCount(1)

      def memberFoo = classATypeDecl.expandAst(NodeTypes.MEMBER)
      memberFoo.checkNodeCount(1)
      memberFoo.checkProperty(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq("code.js::program:ClassA:foo"))
      memberFoo.checkProperty(PropertyNames.CODE, "foo() {}")
    }

    "have method for non-static method in ClassA AST" in AstFixture("""
        |var x = class ClassA {
        |  foo() {}
        |}""".stripMargin) { cpg =>
      def classATypeDecl =
        cpg.typeDecl.nameExact("ClassA").filter(PropertyNames.FULL_NAME, "code.js::program:ClassA")
      classATypeDecl.checkNodeCount(1)

      def methodFoo = classATypeDecl.expandAst(NodeTypes.METHOD).filter(PropertyNames.NAME, "foo")
      methodFoo.checkNodeCount(1)
      methodFoo.checkProperty(PropertyNames.FULL_NAME, "code.js::program:ClassA:foo")
      methodFoo.checkProperty(PropertyNames.CODE, "foo() {}")
    }

    "have TYPE_REF to <meta> for ClassA" in AstFixture("var x = class ClassA {}") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def assignmentToTmp =
        programBlock.expandAst(NodeTypes.CALL)
      assignmentToTmp.checkNodeCount(1)

      def rhs = assignmentToTmp.expandAst(NodeTypes.TYPE_REF)
      rhs.checkNodeCount(1)
      rhs.checkProperty(PropertyNames.TYPE_FULL_NAME, "code.js::program:ClassA<meta>")
    }

    "have correct structure for type decls for classes with extends" in AstFixture("class ClassA extends Base {}") {
      cpg =>
        def classATypeDecl =
          cpg.typeDecl.nameExact("ClassA").filter(PropertyNames.NAME, "ClassA")
        classATypeDecl.checkNodeCount(1)
        classATypeDecl.checkProperty(PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, Seq("Base"))
    }
  }

  "AST generation for constructor" should {
    "have correct structure for simple new" in AstFixture("new MyClass()") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def newCallBlock =
        programBlock
          .expandAst(NodeTypes.BLOCK)
          .filter(PropertyNames.CODE, "new MyClass()")
      newCallBlock.checkNodeCount(1)

      val tmpName = "_tmp_0"

      def localTmp = newCallBlock.expandAst(NodeTypes.LOCAL)
      localTmp.checkNodeCount(1)
      localTmp.checkProperty(PropertyNames.NAME, tmpName)

      def tmpAssignment =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, tmpName + " = .alloc")
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def tmp = tmpAssignment.expandAst(NodeTypes.IDENTIFIER)
      tmp.checkNodeCount(1)
      tmp.checkProperty(PropertyNames.CODE, tmpName)
      tmp.checkProperty(PropertyNames.NAME, tmpName)

      def allocCall = tmpAssignment.expandAst(NodeTypes.CALL)
      allocCall.checkNodeCount(1)
      allocCall.checkProperty(PropertyNames.NAME, Operators.alloc)
      allocCall.checkProperty(PropertyNames.CODE, ".alloc")

      def constructorCall =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "MyClass()")
      constructorCall.checkNodeCount(1)

      def name =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "MyClass")
      name.checkNodeCount(1)

      def receiver =
        constructorCall.expandReceiver(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "MyClass")
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def tmpArg0 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, tmpName)
      tmpArg0.checkNodeCount(1)
      tmpArg0.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def tmpArg0Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, tmpName)
      tmpArg0Argument.checkNodeCount(1)
      tmpArg0Argument.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def returnTmp = newCallBlock.expandAst(NodeTypes.IDENTIFIER)
      returnTmp.checkNodeCount(1)
      returnTmp.checkProperty(PropertyNames.NAME, tmpName)
    }

    "have correct structure for simple new with arguments" in AstFixture("new MyClass(arg1, arg2)") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def newCallBlock =
        programBlock
          .expandAst(NodeTypes.BLOCK)
          .filter(PropertyNames.CODE, "new MyClass(arg1, arg2)")
      newCallBlock.checkNodeCount(1)

      val tmpName = "_tmp_0"

      def localTmp = newCallBlock.expandAst(NodeTypes.LOCAL)
      localTmp.checkNodeCount(1)
      localTmp.checkProperty(PropertyNames.NAME, tmpName)

      def tmpAssignment =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, tmpName + " = .alloc")
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def tmp = tmpAssignment.expandAst(NodeTypes.IDENTIFIER)
      tmp.checkNodeCount(1)
      tmp.checkProperty(PropertyNames.CODE, tmpName)
      tmp.checkProperty(PropertyNames.NAME, tmpName)

      def allocCall = tmpAssignment.expandAst(NodeTypes.CALL)
      allocCall.checkNodeCount(1)
      allocCall.checkProperty(PropertyNames.NAME, Operators.alloc)
      allocCall.checkProperty(PropertyNames.CODE, ".alloc")

      def constructorCall =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "MyClass(arg1, arg2)")
      constructorCall.checkNodeCount(1)

      def name =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "MyClass")
      name.checkNodeCount(1)

      def receiver =
        constructorCall.expandReceiver(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "MyClass")
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def tmpArg0 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, tmpName)
      tmpArg0.checkNodeCount(1)
      tmpArg0.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def tmpArg0Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, tmpName)
      tmpArg0Argument.checkNodeCount(1)
      tmpArg0Argument.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def arg1 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "arg1")
      arg1.checkNodeCount(1)
      arg1.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

      def arg1Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "arg1")
      arg1Argument.checkNodeCount(1)
      arg1Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

      def arg2 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "arg2")
      arg2.checkNodeCount(1)
      arg2.checkProperty(PropertyNames.ARGUMENT_INDEX, 3)

      def arg2Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "arg2")
      arg2Argument.checkNodeCount(1)
      arg2Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 3)

      def returnTmp = newCallBlock.expandAst(NodeTypes.IDENTIFIER)
      returnTmp.checkNodeCount(1)
      returnTmp.checkProperty(PropertyNames.NAME, tmpName)
    }

    "have correct structure for new with access path" in AstFixture("new foo.bar.MyClass()") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def programBlock = program.expandAst(NodeTypes.BLOCK)
      programBlock.checkNodeCount(1)

      def newCallBlock =
        programBlock
          .expandAst(NodeTypes.BLOCK)
          .filter(PropertyNames.CODE, "new foo.bar.MyClass()")
      newCallBlock.checkNodeCount(1)

      val tmpName = "_tmp_0"

      def localTmp = newCallBlock.expandAst(NodeTypes.LOCAL)
      localTmp.checkNodeCount(1)
      localTmp.checkProperty(PropertyNames.NAME, tmpName)

      def tmpAssignment =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, tmpName + " = .alloc")
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def tmp = tmpAssignment.expandAst(NodeTypes.IDENTIFIER)
      tmp.checkNodeCount(1)
      tmp.checkProperty(PropertyNames.CODE, tmpName)
      tmp.checkProperty(PropertyNames.NAME, tmpName)

      def allocCall = tmpAssignment.expandAst(NodeTypes.CALL)
      allocCall.checkNodeCount(1)
      allocCall.checkProperty(PropertyNames.NAME, Operators.alloc)
      allocCall.checkProperty(PropertyNames.CODE, ".alloc")

      def constructorCall =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "foo.bar.MyClass()")
      constructorCall.checkNodeCount(1)

      def path =
        constructorCall.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "foo.bar.MyClass")
      path.checkProperty(PropertyNames.NAME, Operators.fieldAccess)
      path.checkNodeCount(1)

      def receiver =
        constructorCall
          .expandReceiver(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "foo.bar.MyClass")
      receiver.checkProperty(PropertyNames.NAME, Operators.fieldAccess)
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def tmpArg0 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, tmpName)
      tmpArg0.checkNodeCount(1)
      tmpArg0.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def tmpArg0Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, tmpName)
      tmpArg0Argument.checkNodeCount(1)
      tmpArg0Argument.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def returnTmp = newCallBlock.expandAst(NodeTypes.IDENTIFIER)
      returnTmp.checkNodeCount(1)
      returnTmp.checkProperty(PropertyNames.NAME, tmpName)
    }

    "have correct structure for throw new exceptions" in AstFixture("function foo() { throw new Foo() }") { cpg =>
      def fooBlock = cpg.method
        .nameExact("foo")
        .expandAst(NodeTypes.BLOCK)
      fooBlock.checkNodeCount(1)

      def throwCall =
        fooBlock
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "throw new Foo()")
      throwCall.checkNodeCount(1)
      throwCall.checkProperty(PropertyNames.NAME, "<operator>.throw")

      def newCallBlock =
        throwCall
          .expandAst(NodeTypes.BLOCK)
          .filter(PropertyNames.CODE, "new Foo()")
      newCallBlock.checkNodeCount(1)

      val tmpName = "_tmp_0"

      def localTmp = newCallBlock.expandAst(NodeTypes.LOCAL)
      localTmp.checkNodeCount(1)
      localTmp.checkProperty(PropertyNames.NAME, tmpName)

      def tmpAssignment =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, tmpName + " = .alloc")
      tmpAssignment.checkNodeCount(1)
      tmpAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

      def tmp = tmpAssignment.expandAst(NodeTypes.IDENTIFIER)
      tmp.checkNodeCount(1)
      tmp.checkProperty(PropertyNames.CODE, tmpName)
      tmp.checkProperty(PropertyNames.NAME, tmpName)

      def allocCall = tmpAssignment.expandAst(NodeTypes.CALL)
      allocCall.checkNodeCount(1)
      allocCall.checkProperty(PropertyNames.NAME, Operators.alloc)
      allocCall.checkProperty(PropertyNames.CODE, ".alloc")

      def constructorCall =
        newCallBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "Foo()")
      constructorCall.checkNodeCount(1)

      def name =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "Foo")
      name.checkNodeCount(1)

      def receiver =
        constructorCall.expandReceiver(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "Foo")
      receiver.checkNodeCount(1)
      receiver.checkProperty(PropertyNames.ORDER, 0)

      def tmpArg0 =
        constructorCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, tmpName)
      tmpArg0.checkNodeCount(1)
      tmpArg0.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def tmpArg0Argument =
        constructorCall
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, tmpName)
      tmpArg0Argument.checkNodeCount(1)
      tmpArg0Argument.checkProperty(PropertyNames.ORDER, 1)
      tmpArg0Argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

      def returnTmp = newCallBlock.expandAst(NodeTypes.IDENTIFIER)
      returnTmp.checkNodeCount(1)
      returnTmp.checkProperty(PropertyNames.NAME, tmpName)
    }
  }

  "AST generation for await/async" should {
    "have correct structure for await/async" in AstFixture("async function x(foo) { await foo() }") { cpg =>
      def awaitCall =
        cpg.method
          .nameExact("x")
          .expandAst(NodeTypes.BLOCK)
          .expandAst(NodeTypes.CALL)
      awaitCall.checkNodeCount(1)
      awaitCall.checkProperty(PropertyNames.CODE, "await foo()")
      awaitCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)
      awaitCall.checkProperty(PropertyNames.METHOD_FULL_NAME, "<operator>.await")

      def fooCall = awaitCall.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "foo()")
      fooCall.checkNodeCount(1)
    }
  }

  "AST generation for instanceof/delete" should {
    "have correct structure for instanceof" in AstFixture("x instanceof Foo") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def instanceOf =
        program
          .expandAst(NodeTypes.BLOCK)
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "x instanceof Foo")
      instanceOf.checkNodeCount(1)
      instanceOf.checkProperty(PropertyNames.NAME, Operators.instanceOf)

      def lhs = instanceOf.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
      lhs.checkNodeCount(1)
      lhs.checkProperty(PropertyNames.CODE, "x")
      def lhsArg =
        instanceOf.expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "x")
      lhsArg.checkNodeCount(1)
      lhsArg.checkProperty(PropertyNames.CODE, "x")

      def rhs = instanceOf.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "Foo")
      rhs.checkNodeCount(1)
      rhs.checkProperty(PropertyNames.CODE, "Foo")
      def rhsArg =
        instanceOf
          .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
          .filter(PropertyNames.NAME, "Foo")
      rhsArg.checkNodeCount(1)
      rhsArg.checkProperty(PropertyNames.CODE, "Foo")
    }

    "have correct structure for delete" in AstFixture("delete foo.x") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def delete =
        program
          .expandAst(NodeTypes.BLOCK)
          .expandAst(NodeTypes.CALL)
          .filter(PropertyNames.CODE, "delete foo.x")
      delete.checkNodeCount(1)
      delete.checkProperty(PropertyNames.NAME, Operators.delete)

      def rhs = delete.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, Operators.fieldAccess)
      rhs.checkNodeCount(1)
      rhs.checkProperty(PropertyNames.CODE, "foo.x")
    }
  }

  "AST generation for default parameters" should {
    "have correct structure for method parameter with default" in AstFixture("function foo(a = 1) {}") { cpg =>
      def foo = cpg.method.nameExact("foo")
      foo.checkNodeCount(1)

      def paramA = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "a")
      paramA.checkNodeCount(1)
      paramA.checkProperty(PropertyNames.INDEX, 1)

      def block = foo.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def assignment = block.expandAst(NodeTypes.CALL)
      assignment.checkNodeCount(1)

      def a = assignment.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      a.checkNodeCount(1)

      def ternaryCall =
        assignment.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
      ternaryCall.checkNodeCount(1)

      def testCall =
        ternaryCall.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
      testCall.checkNodeCount(1)

      def testCallLhs = testCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      testCallLhs.checkNodeCount(1)

      def testCallRhs =
        testCall.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
      testCallRhs.checkNodeCount(1)

      def trueCase = ternaryCall.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      trueCase.checkNodeCount(1)

      def falseCase = ternaryCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      falseCase.checkNodeCount(1)
    }

    "have correct structure for multiple method parameters with default" in AstFixture(
      "function foo(a = 1, b = 2) {}"
    ) { cpg =>
      def foo = cpg.method.nameExact("foo")
      foo.checkNodeCount(1)

      def paramA = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "a")
      paramA.checkNodeCount(1)
      paramA.checkProperty(PropertyNames.INDEX, 1)

      def paramB = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "b")
      paramB.checkNodeCount(1)
      paramB.checkProperty(PropertyNames.INDEX, 2)

      def block = foo.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def assignmentA =
        block.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "a = a === void 0 ? 1 : a")
      assignmentA.checkNodeCount(1)

      def a = assignmentA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      a.checkNodeCount(1)

      def ternaryCallA =
        assignmentA.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
      ternaryCallA.checkNodeCount(1)

      def testCallA =
        ternaryCallA.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
      testCallA.checkNodeCount(1)

      def testCallALhs = testCallA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      testCallALhs.checkNodeCount(1)

      def testCallARhs =
        testCallA.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
      testCallARhs.checkNodeCount(1)

      def trueCaseA = ternaryCallA.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      trueCaseA.checkNodeCount(1)

      def falseCaseA = ternaryCallA.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "a")
      falseCaseA.checkNodeCount(1)

      def assignmentB =
        block.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = b === void 0 ? 2 : b")
      assignmentB.checkNodeCount(1)

      def b = assignmentB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      b.checkNodeCount(1)

      def ternaryCallB =
        assignmentB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
      ternaryCallB.checkNodeCount(1)

      def testCallB =
        ternaryCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
      testCallB.checkNodeCount(1)

      def testCallBLhs = testCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      testCallBLhs.checkNodeCount(1)

      def testCallBRhs =
        testCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
      testCallBRhs.checkNodeCount(1)

      def trueCaseB = ternaryCallB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
      trueCaseB.checkNodeCount(1)

      def falseCaseB = ternaryCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      falseCaseB.checkNodeCount(1)
    }

    "have correct structure for method mixed parameters with default" in AstFixture("function foo(a, b = 1) {}") {
      cpg =>
        def foo = cpg.method.nameExact("foo")
        foo.checkNodeCount(1)

        def paramA = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "a")
        paramA.checkNodeCount(1)
        paramA.checkProperty(PropertyNames.INDEX, 1)

        def paramB = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "b")
        paramB.checkNodeCount(1)
        paramB.checkProperty(PropertyNames.INDEX, 2)

        def block = foo.expandAst(NodeTypes.BLOCK)
        block.checkNodeCount(1)

        def assignmentB =
          block.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = b === void 0 ? 1 : b")
        assignmentB.checkNodeCount(1)

        def b = assignmentB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
        b.checkNodeCount(1)

        def ternaryCallB =
          assignmentB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
        ternaryCallB.checkNodeCount(1)

        def testCallB =
          ternaryCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
        testCallB.checkNodeCount(1)

        def testCallBLhs = testCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
        testCallBLhs.checkNodeCount(1)

        def testCallBRhs =
          testCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
        testCallBRhs.checkNodeCount(1)

        def trueCaseB = ternaryCallB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
        trueCaseB.checkNodeCount(1)

        def falseCaseB = ternaryCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
        falseCaseB.checkNodeCount(1)
    }

    "have correct structure for multiple method mixed parameters with default" in AstFixture(
      "function foo(a, b = 1, c = 2) {}"
    ) { cpg =>
      def foo = cpg.method.nameExact("foo")
      foo.checkNodeCount(1)

      def paramA = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "a")
      paramA.checkNodeCount(1)
      paramA.checkProperty(PropertyNames.INDEX, 1)

      def paramB = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "b")
      paramB.checkNodeCount(1)
      paramB.checkProperty(PropertyNames.INDEX, 2)

      def paramC = foo.expandAst(NodeTypes.METHOD_PARAMETER_IN).filter(PropertyNames.NAME, "c")
      paramC.checkNodeCount(1)
      paramC.checkProperty(PropertyNames.INDEX, 3)

      def block = foo.expandAst(NodeTypes.BLOCK)
      block.checkNodeCount(1)

      def assignmentB =
        block.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "b = b === void 0 ? 1 : b")
      assignmentB.checkNodeCount(1)

      def b = assignmentB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      b.checkNodeCount(1)

      def ternaryCallB =
        assignmentB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
      ternaryCallB.checkNodeCount(1)

      def testCallB =
        ternaryCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
      testCallB.checkNodeCount(1)

      def testCallBLhs = testCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      testCallBLhs.checkNodeCount(1)

      def testCallBRhs =
        testCallB.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
      testCallBRhs.checkNodeCount(1)

      def trueCaseB = ternaryCallB.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "1")
      trueCaseB.checkNodeCount(1)

      def falseCaseB = ternaryCallB.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "b")
      falseCaseB.checkNodeCount(1)

      def assignmentC =
        block.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "c = c === void 0 ? 2 : c")
      assignmentC.checkNodeCount(1)

      def c = assignmentC.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "c")
      c.checkNodeCount(1)

      def ternaryCallC =
        assignmentC.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.conditional")
      ternaryCallC.checkNodeCount(1)

      def testCallC =
        ternaryCallC.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.equals")
      testCallC.checkNodeCount(1)

      def testCallCLhs = testCallC.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "c")
      testCallCLhs.checkNodeCount(1)

      def testCallCRhs =
        testCallC.expandAst(NodeTypes.CALL).filter(PropertyNames.NAME, "<operator>.void")
      testCallCRhs.checkNodeCount(1)
      testCallCRhs.checkNodeCount(1)

      def trueCaseC = ternaryCallC.expandAst(NodeTypes.LITERAL).filter(PropertyNames.CODE, "2")
      trueCaseC.checkNodeCount(1)

      def falseCaseC = ternaryCallC.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "c")
      falseCaseC.checkNodeCount(1)
    }
  }

  "AST generation for global builtins" should {
    "have correct structure for JSON.parse" in AstFixture("""JSON.parse("foo");""") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def methodBlock = program.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def parseCall = methodBlock.expandAst(NodeTypes.CALL)
      parseCall.checkNodeCount(1)
      parseCall.checkProperty(PropertyNames.NAME, "parse")
      parseCall.checkProperty(PropertyNames.METHOD_FULL_NAME, "JSON.parse")
      parseCall.checkProperty(PropertyNames.CODE, """JSON.parse("foo")""")
      parseCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)

      def argument = parseCall.expandAst().filter(PropertyNames.CODE, """"foo"""")
      argument.checkNodeCount(1)
      argument.checkProperty(PropertyNames.ORDER, 1)
      argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
    }

    "have correct structure for JSON.stringify" in AstFixture("""JSON.stringify(foo);""") { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def methodBlock = program.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def parseCall = methodBlock.expandAst(NodeTypes.CALL)
      parseCall.checkNodeCount(1)
      parseCall.checkProperty(PropertyNames.NAME, "stringify")
      parseCall.checkProperty(PropertyNames.METHOD_FULL_NAME, "JSON.stringify")
      parseCall.checkProperty(PropertyNames.CODE, "JSON.stringify(foo)")
      parseCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)

      def argument = parseCall.expandAst().filter(PropertyNames.NAME, "foo")
      argument.checkNodeCount(1)
      argument.checkProperty(PropertyNames.CODE, "foo")
      argument.checkProperty(PropertyNames.ORDER, 1)
      argument.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)
    }

    "not create static builtin call for calls not exactly matching dictionary" in AstFixture(
      """JSON.parse.apply("foo");"""
    ) { cpg =>
      def program = cpg.method.nameExact(":program")
      program.checkNodeCount(1)

      def methodBlock = program.expandAst(NodeTypes.BLOCK)
      methodBlock.checkNodeCount(1)

      def parseCall = methodBlock.expandAst(NodeTypes.CALL)
      parseCall.checkNodeCount(1)
      parseCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.DYNAMIC_DISPATCH)
    }
  }

  "AST generation for dependencies" should {
    "have no dependencies if none are declared at all" in AstFixture("var x = 1;") { cpg =>
      getDependencies(cpg).l.size shouldBe 0
    }

    "have correct dependencies (imports)" in AstFixture("""
        |import {a} from "depA";
        |import {b} from "depB";
        |""".stripMargin) { cpg =>
      getDependencies(cpg).checkNodeCount(2)

      def depA = getDependencies(cpg).filter(PropertyNames.NAME, "a")
      depA.checkNodeCount(1)
      depA.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depA")

      def depB = getDependencies(cpg).filter(PropertyNames.NAME, "b")
      depB.checkNodeCount(1)
      depB.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depB")
    }

    "have correct import nodes" in AstFixture("""
        |import {a} from "depA";
        |import {b} from "depB";
        |import {c} from "";
        |import * as d from "depD";
        |""".stripMargin) { cpg =>
      val List(a: Import, b: Import, c: Import, d: Import) = getImports(cpg).l
      a.code shouldBe "import {a} from \"depA\""
      a.importedEntity shouldBe Some("depA")
      a.importedAs shouldBe Some("a")
      b.code shouldBe "import {b} from \"depB\""
      b.importedEntity shouldBe Some("depB")
      b.importedAs shouldBe Some("b")
      c.code shouldBe "import {c} from \"\""
      c.importedEntity should not be defined
      c.importedAs shouldBe Some("c")
      d.code shouldBe "import * as d from \"depD\""
      d.importedEntity shouldBe Some("depD")
      d.importedAs shouldBe Some("d")
      a.astIn.l match {
        case List(n: NamespaceBlock) =>
          n.fullName shouldBe "code.js:<global>"
        case _ => fail()
      }
    }

    "have correct dependencies (require)" in AstFixture("""
        |const a = require("depA");
        |const b = require("depB");
        |""".stripMargin) { cpg =>
      getDependencies(cpg).checkNodeCount(2)

      def depA = getDependencies(cpg).filter(PropertyNames.NAME, "a")
      depA.checkNodeCount(1)
      depA.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depA")
      depA.checkProperty(PropertyNames.VERSION, "require")

      def depB = getDependencies(cpg).filter(PropertyNames.NAME, "b")
      depB.checkNodeCount(1)
      depB.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depB")
      depB.checkProperty(PropertyNames.VERSION, "require")
    }

    "have correct dependencies (strange requires)" in AstFixture("""
        |var _ = require("depA");
        |var b = require("depB").some.strange().call().here;
        |var { c } = require('depC');
        |var { d, e } = require('depD');
        |var [ f, g ] = require('depE');
        |""".stripMargin) { cpg =>
      getDependencies(cpg).checkNodeCount(7)

      def depA = getDependencies(cpg).filter(PropertyNames.NAME, "_")
      depA.checkNodeCount(1)
      depA.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depA")
      depA.checkProperty(PropertyNames.VERSION, "require")

      def depB = getDependencies(cpg).filter(PropertyNames.NAME, "b")
      depB.checkNodeCount(1)
      depB.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depB")
      depB.checkProperty(PropertyNames.VERSION, "require")

      def depC = getDependencies(cpg).filter(PropertyNames.NAME, "c")
      depC.checkNodeCount(1)
      depC.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depC")
      depC.checkProperty(PropertyNames.VERSION, "require")

      def depD = getDependencies(cpg).filter(PropertyNames.NAME, "d")
      depD.checkNodeCount(1)
      depD.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depD")
      depD.checkProperty(PropertyNames.VERSION, "require")

      def depE = getDependencies(cpg).filter(PropertyNames.NAME, "e")
      depE.checkNodeCount(1)
      depE.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depD")
      depE.checkProperty(PropertyNames.VERSION, "require")

      def depF = getDependencies(cpg).filter(PropertyNames.NAME, "f")
      depF.checkNodeCount(1)
      depF.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depE")
      depF.checkProperty(PropertyNames.VERSION, "require")

      def depG = getDependencies(cpg).filter(PropertyNames.NAME, "g")
      depG.checkNodeCount(1)
      depG.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depE")
      depG.checkProperty(PropertyNames.VERSION, "require")
    }

    "have correct dependencies (mixed)" in AstFixture("""
        |import {a} from "depA";
        |const b = require("depB");
        |""".stripMargin) { cpg =>
      getDependencies(cpg).checkNodeCount(2)

      def depA = getDependencies(cpg).filter(PropertyNames.NAME, "a")
      depA.checkNodeCount(1)
      depA.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depA")

      def depB = getDependencies(cpg).filter(PropertyNames.NAME, "b")
      depB.checkNodeCount(1)
      depB.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "depB")
    }

    "have correct dependencies (different variations of import)" in AstFixture("""
       |import name from "module-name";
       |import * as otherName from "module-name";
       |import { member1 } from "module-name";
       |import { member2 as alias1 } from "module-name";
       |import { member3 , member4 } from "module-name";
       |import { member5 , member6 as alias2 } from "module-name";
       |import defaultMember1, * as alias3 from "module-name";
       |import defaultMember2 from "module-name";
       |import "module-name";
       |""".stripMargin) { cpg =>
      def deps = getDependencies(cpg)
      deps.checkNodeCount(12)

      deps
        .filter(PropertyNames.NAME, "name")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "otherName")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "member1")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "alias1")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "member3")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "member4")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "member5")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "alias2")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "defaultMember1")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "alias3")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "defaultMember2")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)

      deps
        .filter(PropertyNames.NAME, "module-name")
        .filter(PropertyNames.DEPENDENCY_GROUP_ID, "module-name")
        .checkNodeCount(1)
    }
  }

  "AST generation for exports" should {
    "have correct structure for simple names and aliases" in AstFixture("""
        |var name1, name2, name3, name6;
        |var variable4, variable5;
        |export { name1, name2, name3 };
        |export { variable4 as name4, variable5 as name5, name6 };
        |export let name7, name8, name9;
        |export let name10 = "10", name11 = "11", name12;
        |""".stripMargin) { cpg =>
      cpg.local.code.l shouldBe List(
        "name1",
        "name2",
        "name3",
        "name6",
        "variable4",
        "variable5",
        "name7",
        "name8",
        "name9",
        "name10",
        "name11",
        "name12"
      )
      cpg.call(Operators.assignment).code.l shouldBe List(
        "exports.name1 = name1",
        "exports.name2 = name2",
        "exports.name3 = name3",
        "exports.name4 = variable4",
        "exports.name5 = variable5",
        "exports.name6 = name6",
        "exports.name7 = name7",
        "exports.name8 = name8",
        "exports.name9 = name9",
        "name10 = \"10\"",
        "name11 = \"11\"",
        "exports.name10 = name10",
        "exports.name11 = name11",
        "exports.name12 = name12"
      )
    }

    "have correct structure export assignments" in AstFixture("""
        |var foo = 1;
        |var bar = 2;
        |export = foo;
        |export = bar;
        |""".stripMargin) { cpg =>
      cpg.local.code.l shouldBe List("foo", "bar")
      cpg.call(Operators.assignment).code.l shouldBe List(
        "foo = 1",
        "bar = 2",
        "exports.foo = foo",
        "exports.bar = bar"
      )
    }

    "have correct structure for defaults" in AstFixture("""
        |var name1;
        |export { name1 as default };
        |export default name2 = "2";
        |export default function foo(param) {};
        |""".stripMargin) { cpg =>
      cpg.local.code.l shouldBe List("name1", "foo", "name2")
      cpg.call(Operators.assignment).code.l shouldBe List(
        "exports[\"default\"] = name1",
        "name2 = \"2\"",
        "exports[\"default\"] = name2",
        "function foo = function foo(param) {}",
        "exports[\"default\"] = foo"
      )
      cpg.method("foo").code.l shouldBe List("function foo(param) {}")
    }

    "have correct structure for export with from clause" in AstFixture("""
      |export { import1 as name1, import2 as name2, name3 } from "Foo";
      |export bar from "Bar";
      |""".stripMargin) { cpg =>
      def dep1 = getDependencies(cpg).filter(PropertyNames.NAME, "name1")
      dep1.checkNodeCount(1)
      dep1.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Foo")
      dep1.checkProperty(PropertyNames.VERSION, "require")

      def dep2 = getDependencies(cpg).filter(PropertyNames.NAME, "name2")
      dep2.checkNodeCount(1)
      dep2.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Foo")
      dep2.checkProperty(PropertyNames.VERSION, "require")

      def dep3 = getDependencies(cpg).filter(PropertyNames.NAME, "name3")
      dep3.checkNodeCount(1)
      dep3.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Foo")
      dep3.checkProperty(PropertyNames.VERSION, "require")

      def dep4 = getDependencies(cpg).filter(PropertyNames.NAME, "bar")
      dep4.checkNodeCount(1)
      dep4.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Bar")
      dep4.checkProperty(PropertyNames.VERSION, "require")

      cpg.call(Operators.assignment).code.l shouldBe List(
        "_Foo = require(\"Foo\")",
        "_Foo.name1 = import1",
        "_Foo.name2 = import2",
        "_Foo.name3 = name3",
        "_Bar = require(\"Bar\")",
        "_Bar.bar = bar"
      )
    }

    "have correct structure for export all with from clause" in AstFixture("""
       |export * from "Foo";
       |export * as B from "Bar";
       |export * from "./some/Module";
       |""".stripMargin) { cpg =>
      def dep1 = getDependencies(cpg).filter(PropertyNames.NAME, "Foo")
      dep1.checkNodeCount(1)
      dep1.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Foo")
      dep1.checkProperty(PropertyNames.VERSION, "require")

      def dep2 = getDependencies(cpg).filter(PropertyNames.NAME, "B")
      dep2.checkNodeCount(1)
      dep2.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "Bar")
      dep2.checkProperty(PropertyNames.VERSION, "require")

      def dep3 = getDependencies(cpg).filter(PropertyNames.NAME, "Module")
      dep3.checkNodeCount(1)
      dep3.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "./some/Module")
      dep3.checkProperty(PropertyNames.VERSION, "require")

      cpg.call(Operators.assignment).code.l shouldBe List(
        "_Foo = require(\"Foo\")",
        "exports.Foo = _Foo",
        "_Bar = require(\"Bar\")",
        "exports.B = _Bar",
        "_Module = require(\"./some/Module\")",
        "exports.Module = _Module"
      )
    }

  }

  private def checkObjectInitialization(node: Node, member: (String, String)): Unit = {
    def block = Traversal.fromSingle(node)
    block.checkNodeCount(1)

    def keyName       = member._1
    def assignedValue = member._2

    def localTmp = block.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_tmp_0")
    localTmp.checkNodeCount(1)
    localTmp.checkProperty(PropertyNames.ORDER, 0)

    def tmp = block.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_tmp_0")
    tmp.checkNodeCount(1)

    def call =
      block
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, s"_tmp_0.$keyName = $assignedValue")
    call.checkNodeCount(1)

    def tmpAccess = call.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, "_tmp_0." + keyName)
    tmpAccess.checkNodeCount(1)
    tmpAccess.checkProperty(PropertyNames.METHOD_FULL_NAME, Operators.fieldAccess)

    def leftHandSideTmpId =
      tmpAccess.expandAst().filter(PropertyNames.NAME, "_tmp_0")
    leftHandSideTmpId.checkNodeCount(1)
    leftHandSideTmpId.checkProperty(PropertyNames.CODE, "_tmp_0")

    def key =
      tmpAccess.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.CANONICAL_NAME, keyName)
    key.checkNodeCount(1)

    def value = call.expandAst().filter(PropertyNames.CODE, assignedValue)
    value.checkNodeCount(1)
  }

  private def checkForInOrOf(node: Node): Unit = {
    def loopBlock = Traversal.fromSingle(node)
    loopBlock.checkNodeCount(1)

    def localIterator =
      loopBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_iterator_0")
    localIterator.checkNodeCount(1)

    def localResult = loopBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "_result_0")
    localResult.checkNodeCount(1)

    def localI = loopBlock.expandAst(NodeTypes.LOCAL).filter(PropertyNames.NAME, "i")
    localI.checkNodeCount(1)

    def iteratorAssignment =
      loopBlock
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "_iterator_0 = Object.keys(arr)[Symbol.iterator]()")
    iteratorAssignment.checkNodeCount(1)
    iteratorAssignment.checkProperty(PropertyNames.NAME, Operators.assignment)

    def iteratorAssignmentLhs = iteratorAssignment.expandAst(NodeTypes.IDENTIFIER)
    iteratorAssignmentLhs.checkNodeCount(1)
    iteratorAssignmentLhs.checkProperty(PropertyNames.NAME, "_iterator_0")
    iteratorAssignmentLhs.checkProperty(PropertyNames.ORDER, 1)
    iteratorAssignmentLhs.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

    def iteratorAssignmentRhs = iteratorAssignment.expandAst(NodeTypes.CALL)
    iteratorAssignmentRhs.checkNodeCount(1)
    iteratorAssignmentRhs.checkProperty(PropertyNames.CODE, "Object.keys(arr)[Symbol.iterator]()")
    iteratorAssignmentRhs.checkProperty(PropertyNames.ORDER, 2)
    iteratorAssignmentRhs.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

    def indexCall = iteratorAssignmentRhs.expandAst(NodeTypes.CALL)
    indexCall.checkNodeCount(1)
    indexCall.checkProperty(PropertyNames.NAME, Operators.indexAccess)

    def objectKeysCall = indexCall.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 1)
    objectKeysCall.checkNodeCount(1)
    objectKeysCall.checkProperty(PropertyNames.NAME, "keys")
    objectKeysCall.checkProperty(PropertyNames.METHOD_FULL_NAME, "Object.keys")
    objectKeysCall.checkProperty(PropertyNames.CODE, "Object.keys(arr)")
    objectKeysCall.checkProperty(PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH)

    def objectKeysCallArg =
      objectKeysCall
        .expand(EdgeTypes.ARGUMENT, NodeTypes.IDENTIFIER)
        .filter(PropertyNames.ARGUMENT_INDEX, 1)
    objectKeysCallArg.checkNodeCount(1)
    objectKeysCallArg.checkProperty(PropertyNames.NAME, "arr")
    objectKeysCallArg.checkProperty(PropertyNames.ORDER, 1)

    def indexAccessCall = indexCall.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
    indexAccessCall.checkNodeCount(1)
    indexAccessCall.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

    def symbolIdentifier =
      indexAccessCall.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ORDER, 1)
    symbolIdentifier.checkNodeCount(1)
    symbolIdentifier.checkProperty(PropertyNames.NAME, "Symbol")
    symbolIdentifier.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

    def iteratorIdentifier =
      indexAccessCall.expandAst(NodeTypes.FIELD_IDENTIFIER).filter(PropertyNames.ORDER, 2)
    iteratorIdentifier.checkNodeCount(1)
    iteratorIdentifier.checkProperty(PropertyNames.CANONICAL_NAME, "iterator")
    iteratorIdentifier.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

    def varResult =
      loopBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "_result_0")
    varResult.checkNodeCount(1)

    def varI = loopBlock.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.NAME, "i")
    varI.checkNodeCount(1)

    def loop = loopBlock.expandAst(NodeTypes.CONTROL_STRUCTURE)
    loop.checkNodeCount(1)
    loop.checkProperty(PropertyNames.CONTROL_STRUCTURE_TYPE, ControlStructureTypes.WHILE)

    def loopTestCall =
      loop
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "!(_result_0 = _iterator_0.next()).done")
    loopTestCall.checkNodeCount(1)
    loopTestCall.checkProperty(PropertyNames.NAME, Operators.not)
    loopTestCall.checkProperty(PropertyNames.ORDER, 1)

    def doneMaCall =
      loopTestCall
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "(_result_0 = _iterator_0.next()).done")
    doneMaCall.checkNodeCount(1)
    doneMaCall.checkProperty(PropertyNames.NAME, Operators.fieldAccess)

    def doneMaBase =
      doneMaCall
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "(_result_0 = _iterator_0.next())")
    doneMaBase.checkNodeCount(1)
    doneMaBase.checkProperty(PropertyNames.NAME, Operators.assignment)
    doneMaBase.checkProperty(PropertyNames.ORDER, 1)
    doneMaBase.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

    def doneMaBaseLhs = doneMaBase.expandAst(NodeTypes.IDENTIFIER).filter(PropertyNames.ORDER, 1)
    doneMaBaseLhs.checkNodeCount(1)
    doneMaBaseLhs.checkProperty(PropertyNames.NAME, "_result_0")
    doneMaBaseLhs.checkProperty(PropertyNames.ARGUMENT_INDEX, 1)

    def doneMaBaseRhs = doneMaBase.expandAst(NodeTypes.CALL).filter(PropertyNames.ORDER, 2)
    doneMaBaseRhs.checkNodeCount(1)
    doneMaBaseRhs.checkProperty(PropertyNames.CODE, "_iterator_0.next()")
    doneMaBaseRhs.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

    def doneMember =
      doneMaCall
        .expandAst(NodeTypes.FIELD_IDENTIFIER)
        .filter(PropertyNames.CANONICAL_NAME, "done")
    doneMember.checkNodeCount(1)
    doneMember.checkProperty(PropertyNames.ORDER, 2)
    doneMember.checkProperty(PropertyNames.ARGUMENT_INDEX, 2)

    def whileLoopBlock =
      loop
        .expandAst(NodeTypes.BLOCK)
    whileLoopBlock.checkNodeCount(1)
    whileLoopBlock.checkProperty(PropertyNames.ORDER, 2)

    def loopVarAssignmentCall =
      whileLoopBlock
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "i = _result_0.value")
    loopVarAssignmentCall.checkNodeCount(1)
    loopVarAssignmentCall.checkProperty(PropertyNames.NAME, Operators.assignment)
    loopVarAssignmentCall.checkProperty(PropertyNames.ORDER, 1)

    def fooCall =
      whileLoopBlock
        .expandAst(NodeTypes.BLOCK)
        .expandAst(NodeTypes.CALL)
        .filter(PropertyNames.CODE, "foo(i)")
    fooCall.checkNodeCount(1)
  }

  private def checkLiterals(node: Node, element: Int): Unit = {
    def pushBlock = Traversal.fromSingle(node)
    def pushCall =
      pushBlock.expandAst(NodeTypes.CALL).filter(PropertyNames.CODE, s"_tmp_0.push($element)")
    pushCall.checkNodeCount(1)

    def pushCallReceiver = pushCall.expandReceiver(NodeTypes.CALL)
    pushCallReceiver.checkNodeCount(1)
    pushCallReceiver.checkProperty(PropertyNames.NAME, Operators.fieldAccess)
    pushCallReceiver.checkProperty(PropertyNames.ARGUMENT_INDEX, 0)

    def pushCallReceiverBase =
      pushCallReceiver.expand(EdgeTypes.ARGUMENT).filter(PropertyNames.ORDER, 1)
    pushCallReceiverBase.checkNodeCount(1)
    pushCallReceiverBase.checkProperty(PropertyNames.NAME, "_tmp_0")

    def pushCallReceiverMember =
      pushCallReceiver.expand(EdgeTypes.ARGUMENT).filter(PropertyNames.ORDER, 2)
    pushCallReceiverMember.checkNodeCount(1)
    pushCallReceiverMember.checkProperty(PropertyNames.CANONICAL_NAME, "push")

    def pushCallThis = pushCall.expand(EdgeTypes.ARGUMENT).filter(PropertyNames.ARGUMENT_INDEX, 1)
    pushCallThis.checkNodeCount(1)
    pushCallThis.checkProperty(PropertyNames.NAME, "_tmp_0")

    def pushCallArg = pushCall.expand(EdgeTypes.ARGUMENT).filter(PropertyNames.ARGUMENT_INDEX, 2)
    pushCallArg.checkNodeCount(1)
    pushCallArg.checkProperty(PropertyNames.CODE, s"$element")
  }

}
