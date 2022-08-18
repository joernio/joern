package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class JsClassesAstCreationPassTest extends AbstractPassTest {

  "AST generation for classes" should {
    "have a TYPE_DECL and <meta> TYPE_DECL for ClassA" in AstFixture("var x = class ClassA {}") { cpg =>
      cpg.typeDecl.nameExact("ClassA<meta>").fullNameExact("code.js::program:ClassA<meta>").size shouldBe 1
      cpg.typeDecl.nameExact("ClassA").fullNameExact("code.js::program:ClassA")
    }

    "have constructor binding in <meta> TYPE_DECL for ClassA" in AstFixture("""
        |var x = class ClassA {
        |  constructor() {}
        |}""".stripMargin) { cpg =>
      val List(classAMetaTypeDecl) =
        cpg.typeDecl.nameExact("ClassA<meta>").fullNameExact("code.js::program:ClassA<meta>").l
      val List(constructorBinding) = classAMetaTypeDecl.bindsOut.l
      constructorBinding.name shouldBe ""
      constructorBinding.signature shouldBe ""
      val List(boundMethod) = constructorBinding.refOut.l
      boundMethod.fullName shouldBe "code.js::program:ClassA<constructor>"
      boundMethod.code shouldBe "constructor() {}"
    }

    "have member for static method in <meta> TYPE_DECL for ClassA" in AstFixture("""
       |var x = class ClassA {
       |  static staticFoo() {}
       |}""".stripMargin) { cpg =>
      val List(classAMetaTypeDecl) =
        cpg.typeDecl.nameExact("ClassA<meta>").fullNameExact("code.js::program:ClassA<meta>").l
      val List(memberFoo) = classAMetaTypeDecl.member.l
      memberFoo.dynamicTypeHintFullName shouldBe Seq("code.js::program:ClassA:staticFoo")
      memberFoo.code shouldBe "static staticFoo() {}"
    }

    "have method for static method in ClassA AST" in AstFixture("""
        |var x = class ClassA {
        |  static staticFoo() {}
        |}""".stripMargin) { cpg =>
      val List(classATypeDecl) =
        cpg.typeDecl.nameExact("ClassA").fullNameExact("code.js::program:ClassA").l

      val List(methodFoo) = classATypeDecl.method.nameExact("staticFoo").l
      methodFoo.fullName shouldBe "code.js::program:ClassA:staticFoo"
      methodFoo.code shouldBe "static staticFoo() {}"
    }

    "have member for non-static method in TYPE_DECL for ClassA" in AstFixture("""
        |var x = class ClassA {
        |  foo() {}
        |}""".stripMargin) { cpg =>
      val List(classATypeDecl) = cpg.typeDecl.nameExact("ClassA").fullNameExact("code.js::program:ClassA").l
      val List(memberFoo)      = classATypeDecl.member.l
      memberFoo.dynamicTypeHintFullName shouldBe Seq("code.js::program:ClassA:foo")
      memberFoo.code shouldBe "foo() {}"
    }

    "have method for non-static method in ClassA AST" in AstFixture("""
        |var x = class ClassA {
        |  foo() {}
        |}""".stripMargin) { cpg =>
      val List(classATypeDecl) = cpg.typeDecl.nameExact("ClassA").fullNameExact("code.js::program:ClassA").l
      val List(methodFoo)      = classATypeDecl.method.nameExact("foo").l
      methodFoo.fullName shouldBe "code.js::program:ClassA:foo"
      methodFoo.code shouldBe "foo() {}"
    }

    "have TYPE_REF to <meta> for ClassA" in AstFixture("var x = class ClassA {}") { cpg =>
      val List(program)         = cpg.method.nameExact(":program").l
      val List(programBlock)    = program.astChildren.isBlock.l
      val List(assignmentToTmp) = programBlock.astChildren.isCall.l
      val List(rhs)             = assignmentToTmp._typeRefViaAstOut.l
      rhs.typeFullName shouldBe "code.js::program:ClassA<meta>"
    }

    "have correct structure for type decls for classes with extends" in AstFixture("class ClassA extends Base {}") {
      cpg =>
        val List(classATypeDecl) =
          cpg.typeDecl.nameExact("ClassA").nameExact("ClassA").l
        classATypeDecl.inheritsFromTypeFullName shouldBe Seq("Base")
    }
  }

  "AST generation for constructor" should {
    "have correct structure for simple new" in AstFixture("new MyClass()") { cpg =>
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
      receiver.order shouldBe 0

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.order shouldBe 1
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.order shouldBe 1
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }

    "have correct structure for simple new with arguments" in AstFixture("new MyClass(arg1, arg2)") { cpg =>
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
      receiver.order shouldBe 0

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.order shouldBe 1
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.order shouldBe 1
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

    "have correct structure for new with access path" in AstFixture("new foo.bar.MyClass()") { cpg =>
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
      receiver.order shouldBe 0

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.order shouldBe 1
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.order shouldBe 1
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }

    "have correct structure for throw new exceptions" in AstFixture("function foo() { throw new Foo() }") { cpg =>
      val List(fooBlock)  = cpg.method.nameExact("foo").astChildren.isBlock.l
      val List(throwCall) = fooBlock.astChildren.isCall.codeExact("throw new Foo()").l
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
      receiver.order shouldBe 0

      val List(tmpArg0) = constructorCall.astChildren.isIdentifier.nameExact(tmpName).l
      tmpArg0.order shouldBe 1
      tmpArg0.argumentIndex shouldBe 0

      val List(tmpArg0Argument) = constructorCall.argument.isIdentifier.nameExact(tmpName).l
      tmpArg0Argument.order shouldBe 1
      tmpArg0Argument.argumentIndex shouldBe 0

      val List(returnTmp) = newCallBlock.astChildren.isIdentifier.l
      returnTmp.name shouldBe tmpName
    }
  }

}
