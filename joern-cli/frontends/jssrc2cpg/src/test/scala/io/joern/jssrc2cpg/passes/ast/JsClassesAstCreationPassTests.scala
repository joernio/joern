package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef}
import io.shiftleft.semanticcpg.language.*

class JsClassesAstCreationPassTests extends AstJsSrc2CpgSuite {

  "AST generation for JS classes" should {

    "have ast parent blocks for class locals" in {
      val cpg = code("""
        |var x = source();
        |
        |class Foo {
        |  func() {
        |    sink(x);
        |  }
        |}
        |
        |function source() {
        |  return 1;
        |}
        |
        |function sink(par1) {}
        |""".stripMargin)
      val List(x1, x2) = cpg.local("x").l
      x1.parentBlock should not be empty
      x1.referencingIdentifiers.name.l shouldBe List("x")
      x2.parentBlock should not be empty
      x2.referencingIdentifiers.name.l shouldBe List("x")
    }

    "have a TYPE_DECL for ClassA" in {
      val cpg = code("var x = class ClassA {}")
      cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").size shouldBe 1
    }

    "have a synthetic assignment for ClassA" in {
      val cpg = code("class ClassA {}")
      cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").size shouldBe 1
      inside(cpg.assignment.argument.l) { case List(id: Identifier, constructorRef: MethodRef) =>
        id.name shouldBe "ClassA"
        id.dynamicTypeHintFullName shouldBe List(s"Test0.js::program:ClassA:${Defines.ConstructorMethodName}")
        constructorRef.code shouldBe "constructor() {}"
        constructorRef.typeFullName shouldBe s"Test0.js::program:ClassA:${Defines.ConstructorMethodName}"
        constructorRef.methodFullName shouldBe s"Test0.js::program:ClassA:${Defines.ConstructorMethodName}"
      }
    }

    "have locals / closure bindings for implicit variables from class definitions" in {
      val cpg = code("""
        |class A {}
        |function b() {
        |  new A();
        |}""".stripMargin)
      cpg.typeDecl.nameExact("A").fullNameExact("Test0.js::program:A").size shouldBe 1
      val List(localA) = cpg.method.name(":program").local.name("A").l
      localA.code shouldBe "A"
      val List(funcLocalA) = cpg.method.name("b").local.name("A").l
      funcLocalA.code shouldBe "A"
      funcLocalA.closureBindingId shouldBe Option("Test0.js::program:b:A")
    }

    "have a member function for static method in TYPE_DECL for ClassA" in {
      val cpg = code("""
       |var x = class ClassA {
       |  static staticFoo() {}
       |}""".stripMargin)
      val List(classATypeDecl)         = cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").l
      val List(constructor, staticFoo) = classATypeDecl.method.l
      constructor.fullName shouldBe s"Test0.js::program:ClassA:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      constructor.code shouldBe "constructor() {}"
      constructor.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.CONSTRUCTOR)
      staticFoo.fullName shouldBe "Test0.js::program:ClassA:staticFoo"
      staticFoo.code shouldBe "static staticFoo() {}"
      staticFoo.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.STATIC)
    }

    "have member for non-static method in TYPE_DECL for ClassA" in {
      val cpg = code("""
        |class ClassA {
        |  foo() {}
        |  [Symbol.iterator]() {}
        |}""".stripMargin)
      val List(classATypeDecl)       = cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").l
      val List(constructor, foo, it) = classATypeDecl.method.l
      constructor.fullName shouldBe s"Test0.js::program:ClassA:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      constructor.code shouldBe "constructor() {}"
      constructor.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.CONSTRUCTOR)
      foo.fullName shouldBe "Test0.js::program:ClassA:foo"
      foo.code shouldBe "foo() {}"
      foo.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL)
      it.fullName shouldBe "Test0.js::program:ClassA:Symbol.iterator"
      it.code shouldBe "[Symbol.iterator]() {}"
      it.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL)
    }

    "have member with initialization in TYPE_DECL for ClassA" in {
      val cpg = code("""
        |class ClassA {
        |  a = 1
        |  b = "foo"
        |  static c = true
        |  static d
        |  static {
        |    this.d = false
        |  }
        |  constructor(param1, param2) {
        |    // also register e and f as dynamically declared members
        |    this.e = param1;
        |    this.f = param2;
        |    // chained access should not result in member creation
        |    this.f.g = param2;
        |  }
        |}""".stripMargin)
      val List(classATypeDecl) = cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").l
      val List(a, b, e, f)     = classATypeDecl.member.not(_.isStatic).l
      a.name shouldBe "a"
      a.code shouldBe "a = 1"
      a.lineNumber shouldBe Option(3)
      a.columnNumber shouldBe Option(2)
      b.name shouldBe "b"
      b.code shouldBe """b = "foo""""
      b.lineNumber shouldBe Option(4)
      b.columnNumber shouldBe Option(2)
      e.name shouldBe "e"
      e.code shouldBe "this.e = param1;"
      e.lineNumber shouldBe Option(12)
      e.columnNumber shouldBe Option(4)
      f.name shouldBe "f"
      f.code shouldBe "this.f = param2;"
      f.lineNumber shouldBe Option(13)
      f.columnNumber shouldBe Option(4)

      val List(c, d) = classATypeDecl.member.isStatic.l
      c.name shouldBe "c"
      c.code shouldBe "static c = true"
      d.name shouldBe "d"
      d.code shouldBe "static d"

      val List(clInitMethod)         = classATypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      val List(cInitCall, dInitCall) = clInitMethod.block.assignment.l
      cInitCall.code shouldBe "static c = true"
      dInitCall.code shouldBe "this.d = false"

      val List(constructor) =
        cpg.typeDecl.nameExact("ClassA").method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l
      val List(aInitCall, bInitCall, eInitCall, fInitCall, gCall) = constructor.block.assignment.l
      aInitCall.code shouldBe "a = 1"
      bInitCall.code shouldBe """b = "foo""""
      eInitCall.code shouldBe "this.e = param1"
      fInitCall.code shouldBe "this.f = param2"
      gCall.code shouldBe "this.f.g = param2"
    }

    "have method for non-static method in ClassA AST" in {
      val cpg = code("""
        |var x = class ClassA {
        |  foo() {}
        |}""".stripMargin)
      val List(classATypeDecl) = cpg.typeDecl.nameExact("ClassA").fullNameExact("Test0.js::program:ClassA").l
      val List(methodFoo)      = classATypeDecl.method.nameExact("foo").l
      methodFoo.fullName shouldBe "Test0.js::program:ClassA:foo"
      methodFoo.code shouldBe "foo() {}"
    }

    "have TYPE_REF to ClassA" in {
      val cpg                   = code("var x = class ClassA {}")
      val List(program)         = cpg.method.nameExact(":program").l
      val List(programBlock)    = program.astChildren.isBlock.l
      val List(assignmentToTmp) = programBlock.astChildren.isCall.l
      val List(rhs)             = assignmentToTmp._typeRefViaAstOut.l
      rhs.typeFullName shouldBe "Test0.js::program:ClassA"
    }

    "have correct structure for type decls for classes with extends" in {
      val cpg                  = code("class ClassA extends Base {}")
      val List(classATypeDecl) = cpg.typeDecl.nameExact("ClassA").nameExact("ClassA").l
      classATypeDecl.inheritsFromTypeFullName shouldBe Seq("Base")
    }
  }

  "AST generation for constructor" should {
    "have correct structure for simple new" in {
      val cpg                = code("new MyClass();")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(newCallBlock) = programBlock.astChildren.isBlock.codeExact("new MyClass()").l
      val tmpName            = "_tmp_0"
      val List(localTmp)     = newCallBlock.astChildren.isLocal.l
      localTmp.name shouldBe tmpName

      val List(tmpAssignment) = newCallBlock.astChildren.isCall.codeExact(s"$tmpName = .alloc").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(tmp) = tmpAssignment.astChildren.isIdentifier.l
      tmp.code shouldBe tmpName
      tmp.name shouldBe tmpName

      val List(allocCall) = tmpAssignment.astChildren.isCall.l
      allocCall.name shouldBe Operators.alloc
      allocCall.code shouldBe ".alloc"

      val List(constructorCall) = newCallBlock.astChildren.isCall.codeExact("new MyClass()").l
      constructorCall.name shouldBe "<operator>.new"
      constructorCall.astChildren.isIdentifier.nameExact("MyClass").size shouldBe 1

      val List(receiver) = constructorCall.receiver.isIdentifier.nameExact("MyClass").l
      receiver.argumentIndex shouldBe -1

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }

    "have correct structure for simple new with arguments" in {
      val cpg                = code("new MyClass(arg1, arg2);")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(newCallBlock) = programBlock.astChildren.isBlock.codeExact("new MyClass(arg1, arg2)").l
      val tmpName            = "_tmp_0"

      val List(localTmp) = newCallBlock.astChildren.isLocal.l
      localTmp.name shouldBe tmpName

      val List(tmpAssignment) = newCallBlock.astChildren.isCall.codeExact(s"$tmpName = .alloc").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(tmp) = tmpAssignment.astChildren.isIdentifier.l
      tmp.name shouldBe tmpName
      tmp.code shouldBe tmpName

      val List(allocCall) = tmpAssignment.astChildren.isCall.l
      allocCall.name shouldBe Operators.alloc
      allocCall.code shouldBe ".alloc"

      val List(constructorCall) = newCallBlock.astChildren.isCall.codeExact("new MyClass(arg1, arg2)").l
      constructorCall.astChildren.isIdentifier.nameExact("MyClass").size shouldBe 1

      val List(receiver) = constructorCall.receiver.isIdentifier.nameExact("MyClass").l
      receiver.argumentIndex shouldBe -1

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(arg1) = constructorCall.astChildren.isIdentifier.nameExact("arg1").l
      arg1.argumentIndex shouldBe 1

      val List(arg1Argument) = constructorCall.argument.isIdentifier.nameExact("arg1").l
      arg1Argument.argumentIndex shouldBe 1

      val List(arg2) = constructorCall.astChildren.isIdentifier.nameExact("arg2").l
      arg2.argumentIndex shouldBe 2

      val List(arg2Argument) = constructorCall.argument.isIdentifier.nameExact("arg2").l
      arg2Argument.argumentIndex shouldBe 2

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }

    "have correct structure for new with access path" in {
      val cpg                = code("new foo.bar.MyClass();")
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(newCallBlock) = programBlock.astChildren.isBlock.codeExact("new foo.bar.MyClass()").l
      val tmpName            = "_tmp_0"

      val List(localTmp) = newCallBlock.astChildren.isLocal.l
      localTmp.name shouldBe tmpName

      val List(tmpAssignment) = newCallBlock.astChildren.isCall.codeExact(s"$tmpName = .alloc").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(tmp) = tmpAssignment.astChildren.isIdentifier.l
      tmp.name shouldBe tmpName
      tmp.code shouldBe tmpName

      val List(allocCall) = tmpAssignment.astChildren.isCall.l
      allocCall.name shouldBe Operators.alloc
      allocCall.code shouldBe ".alloc"

      val List(constructorCall) = newCallBlock.astChildren.isCall.codeExact("new foo.bar.MyClass()").l
      constructorCall.name shouldBe "<operator>.new"

      val List(path) = constructorCall.astChildren.isCall.codeExact("foo.bar.MyClass").l
      path.name shouldBe Operators.fieldAccess

      val List(receiver) = constructorCall.receiver.isCall.codeExact("foo.bar.MyClass").l
      receiver.name shouldBe Operators.fieldAccess
      receiver.argumentIndex shouldBe -1

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }

    "have correct structure for throw new exceptions" in {
      val cpg             = code("function foo() { throw new Foo(); }")
      val List(fooBlock)  = cpg.method.nameExact("foo").astChildren.isBlock.l
      val List(throwCall) = fooBlock.astChildren.isCall.codeExact("throw new Foo();").l
      throwCall.name shouldBe "<operator>.throw"

      val List(newCallBlock) = throwCall.astChildren.isBlock.codeExact("new Foo()").l
      val tmpName            = "_tmp_0"

      val List(localTmp) = newCallBlock.astChildren.isLocal.l
      localTmp.name shouldBe tmpName

      val List(tmpAssignment) = newCallBlock.astChildren.isCall.codeExact(s"$tmpName = .alloc").l
      tmpAssignment.name shouldBe Operators.assignment

      val List(tmp) = tmpAssignment.astChildren.isIdentifier.l
      tmp.name shouldBe tmpName
      tmp.code shouldBe tmpName

      val List(allocCall) = tmpAssignment.astChildren.isCall.l
      allocCall.name shouldBe Operators.alloc
      allocCall.code shouldBe ".alloc"

      val List(constructorCall) = newCallBlock.astChildren.isCall.codeExact("new Foo()").l
      constructorCall.name shouldBe "<operator>.new"
      constructorCall.astChildren.isIdentifier.nameExact("Foo").size shouldBe 1

      val List(receiver) = constructorCall.receiver.isIdentifier.nameExact("Foo").l
      receiver.argumentIndex shouldBe -1

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }
  }

}
