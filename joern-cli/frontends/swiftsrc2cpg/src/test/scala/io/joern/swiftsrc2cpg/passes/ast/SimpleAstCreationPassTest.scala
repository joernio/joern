package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTest extends AstSwiftSrc2CpgSuite {

  "AST generation for simple fragments" should {

    "have module modifier at the top level module method node" in {
      val cpg                         = code("")
      val List(method)                = cpg.method.nameExact("<global>").l
      val List(modVirtual, modModule) = method.modifier.l
      modVirtual.modifierType shouldBe ModifierTypes.VIRTUAL
      modVirtual.order shouldBe 0
      modModule.modifierType shouldBe ModifierTypes.MODULE
      modModule.order shouldBe 1
    }

    "contain the correct file nodes" in {
      val cpg                              = code("")
      val List(testFile, builtinTypesFile) = cpg.file.l
      testFile.name shouldBe "Test0.swift"
      testFile.order shouldBe 0
      builtinTypesFile.name shouldBe "builtintypes"
      builtinTypesFile.order shouldBe 0
    }

    "have correct modifier for a function" in {
      val cpg                                     = code("private static func foo() -> {}")
      val List(method)                            = cpg.method.nameExact("foo").l
      val List(modVirtual, modPrivate, modStatic) = method.modifier.l
      modVirtual.modifierType shouldBe ModifierTypes.VIRTUAL
      modVirtual.order shouldBe 0
      modPrivate.modifierType shouldBe ModifierTypes.PRIVATE
      modPrivate.order shouldBe 1
      modStatic.modifierType shouldBe ModifierTypes.STATIC
      modStatic.order shouldBe 2
    }

    "have correct structure for simple variable declarations" in {
      val cpg = code("""
        |let x = 1
        |var y: String = "2"
        |""".stripMargin)
      val List(method)           = cpg.method.nameExact("<global>").l
      val List(assignX, assignY) = method.assignment.l
      assignX.code shouldBe "let x = 1"
      assignY.code shouldBe """var y: String = "2""""
    }

    "have correct types for simple variable declarations" in {
      val cpg = code("""
        |let a = 1
        |let b: Int = 1
        |var c: String! = ""
        |var d: String? = ""
        |""".stripMargin)
      val List(method)     = cpg.method.nameExact("<global>").l
      val List(a, b, c, d) = method.block.ast.isIdentifier.l
      a.typeFullName shouldBe Defines.Any
      b.typeFullName shouldBe Defines.Int
      c.typeFullName shouldBe Defines.String
      d.typeFullName shouldBe Defines.String
      val List(localA, localB, localC, localD) = method.block.local.l
      localA.typeFullName shouldBe Defines.Any
      localB.typeFullName shouldBe Defines.Int
      localC.typeFullName shouldBe Defines.String
      localD.typeFullName shouldBe Defines.String
    }

    "have correct structure for tuple variable declarations" in {
      val cpg = code("""
        |var (a, b): Int = foo()
        |""".stripMargin)
      val List(method)           = cpg.method.nameExact("<global>").l
      val List(assignA, assignB) = method.assignment.l
      assignA.code shouldBe "var a: Int = foo()"
      assignB.code shouldBe "var b: Int = foo()"
      val List(localA, localB) = method.block.local.l
      localA.name shouldBe "a"
      localA.typeFullName shouldBe "Swift.Int"
      localB.name shouldBe "b"
      localB.typeFullName shouldBe "Swift.Int"
    }

    "have corresponding type decl with correct bindings for function" in {
      val cpg            = code("func method() {}")
      val List(typeDecl) = cpg.typeDecl.nameExact("method").l
      typeDecl.fullName shouldBe "Test0.swift:<global>.method:()->ANY"

      val List(binding) = typeDecl.bindsOut.l
      binding.name shouldBe "method"
      binding.methodFullName shouldBe "Test0.swift:<global>.method:()->ANY"
      binding.signature shouldBe "()->ANY"

      val List(boundMethod) = binding.refOut.l
      boundMethod shouldBe cpg.method.nameExact("method").head
    }

    "have correct ref to local for simple let" in {
      val cpg = code("""
          |let x = ""
          |""".stripMargin)
      val List(localX) = cpg.local.nameExact("x").l
      val List(idX)    = cpg.identifier.nameExact("x").l
      idX.refOut.head shouldBe localX
    }

    "have correct closure bindings" in {
      val cpg = code("""
        |func foo() -> {
        |  let x = 1
        |  func bar() -> {
        |    x = 2
        |  }
        |}
        |""".stripMargin)
      val List(fooMethod)      = cpg.method.nameExact("foo").l
      val List(fooBlock)       = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)      = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)         = fooBlock.astChildren.isMethodRef.l
      val List(closureBinding) = barRef.captureOut.l
      closureBinding.closureBindingId shouldBe Option("Test0.swift:<global>.foo.bar:x")
      closureBinding.refOut.head shouldBe fooLocalX
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      val List(barLocals)      = barMethodBlock.astChildren.isLocal.l
      barLocals.closureBindingId shouldBe Option("Test0.swift:<global>.foo.bar:x")

      val List(identifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      identifierX.refOut.head shouldBe barLocals
    }

    "have correct structure for annotated function" in {
      val cpg = code("""
        |@bar(x: "y")
        |func foo() -> {
        |  let x = 1
        |}
        |""".stripMargin)
      inside(cpg.method.nameExact("foo").annotation.l) { case List(bar) =>
        bar.code shouldBe """@bar(x: "y")"""
        bar.name shouldBe "bar"
        bar.fullName shouldBe "bar"
        val List(paramAssignFoo) = bar.parameterAssign.l
        paramAssignFoo.code shouldBe """x: "y""""
        paramAssignFoo.order shouldBe 1
        val List(paramFoo) = paramAssignFoo.parameter.l
        paramFoo.code shouldBe "argument"
        paramFoo.order shouldBe 1
        val List(paramValueFoo) = paramAssignFoo.value.l
        paramValueFoo.code shouldBe """"y""""
        paramValueFoo.order shouldBe 2
        paramValueFoo.argumentIndex shouldBe 2
        paramValueFoo.argumentName shouldBe Some("x")
      }
    }

    "have correct structure for objc annotated class" in {
      val cpg = code("""
          |@objc(Foo)
          |public class Foo {}
          |""".stripMargin)
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case List(objc) =>
        objc.code shouldBe "@objc(Foo)"
        objc.name shouldBe "objc"
        objc.fullName shouldBe "objc"
        val List(paramAssignFoo) = objc.parameterAssign.l
        paramAssignFoo.code shouldBe "Foo"
        paramAssignFoo.order shouldBe 1
        val List(paramFoo) = paramAssignFoo.parameter.l
        paramFoo.code shouldBe "argument"
        paramFoo.order shouldBe 1
        val List(paramValueFoo) = paramAssignFoo.value.l
        paramValueFoo.code shouldBe "Foo"
        paramValueFoo.order shouldBe 2
        paramValueFoo.argumentIndex shouldBe 2
      }
    }

    "have correct structure for nested classes" in {
      val cpg = code("""
          |public class N1 {
          |  pubic class N2 {
          |    public class N3 {}
          |  }
          |}
          |""".stripMargin)
      val List(n1, n2, n3) = cpg.typeDecl.name("N1", "N2", "N3").l
      n1.fullName shouldBe "Test0.swift:<global>.N1"
      n2.fullName shouldBe "Test0.swift:<global>.N1.N2"
      n3.fullName shouldBe "Test0.swift:<global>.N1.N2.N3"

      n3.astIn.l shouldBe List(n2)
      n2.astIn.l shouldBe List(n1)
    }

    "have correct structure for implicit self access" in {
      // Swift does not allow to access any member / function of the outer class instance.
      // Something like `Foo.self.f` in Foo.Bar.bar is not possible.
      // Hence, we do not need to implement or test this here.
      val cpg = code("""
          |class Foo {
          |  let f: String = "f"
          |  class Bar {
          |    let b = "b"
          |    func bar() {
          |      let f: Int = 1
          |      handleB(b)
          |      handleF(f)
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      cpg.typeDecl.nameExact("Bar").member.name.l shouldBe List("b")
      val List(bAccess) = cpg.call.nameExact("handleB").argument.fieldAccess.l
      bAccess.code shouldBe "self.b"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo.Bar"
        fieldName.canonicalName shouldBe "b"
      }
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.Int
        val fLocal = id._localViaRefOut.get
        fLocal.name shouldBe "f"
        fLocal.typeFullName shouldBe Defines.Int
      }
    }

    "have correct structure for implicit self access with shadowing a member in class" in {
      val cpg = code("""
          |class Foo {
          |  let f: String = "f"
          |  func bar() {
          |    let f: Int = 1
          |    handleF(f)
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.Int
        val fLocal = id._localViaRefOut.get
        fLocal.name shouldBe "f"
        fLocal.typeFullName shouldBe Defines.Int
      }
    }

    "have correct structure for implicit self access with shadowing a variable from outer scope" in {
      val cpg = code("""
          |let f: String = "a"
          |class Foo {
          |  let f: String = "b"
          |  func bar() {
          |    handleF(f)
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      val List(bAccess) = cpg.call.nameExact("handleF").argument.fieldAccess.l
      bAccess.code shouldBe "self.f"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo"
        fieldName.canonicalName shouldBe "f"
      }
    }

    "have correct structure for implicit self access from within nested functions" in {
      val cpg = code("""
          |let f: String = "a"
          |class Foo {
          |  let f: String = "b"
          |  func foo() -> String {
          |    func bar() -> String {
          |      return handleF(f)
          |    }
          |    return bar()
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      val List(bAccess) = cpg.call.nameExact("handleF").argument.fieldAccess.l
      bAccess.code shouldBe "self.f"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo"
        fieldName.canonicalName shouldBe "f"
      }

      val List(fooMethod) = cpg.method.nameExact("foo").l
      val List(fooBlock)  = fooMethod.astChildren.isBlock.l
      val List(barRef)    = fooBlock.astChildren.isMethodRef.l
      barRef.captureOut shouldBe empty

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      barMethodBlock.astChildren.isLocal.name("f") shouldBe empty
    }

    "have correct structure for implicit self access from within nested functions with shadowing" in {
      val cpg = code("""
          |let f: String = "a"
          |class Foo {
          |  let f: String = "b"
          |  func foo() -> String {
          |    let f: String = "c"
          |    func bar() -> String {
          |      return handleF(f)
          |    }
          |    return bar()
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.String
        val fLocal = id._localViaRefOut.get
        fLocal.name shouldBe "f"
        fLocal.typeFullName shouldBe Defines.String
      }
      val List(fooMethod)       = cpg.method.nameExact("foo").l
      val List(fooBlock)        = fooMethod.astChildren.isBlock.l
      val List(fooLocalF)       = fooBlock.astChildren.isLocal.nameExact("f").l
      val List(barRef)          = fooBlock.astChildren.isMethodRef.l
      val List(closureBindingF) = barRef.captureOut.l
      closureBindingF.closureBindingId shouldBe Option("Test0.swift:<global>.Foo.foo.bar:f")
      closureBindingF.refOut.head shouldBe fooLocalF
      closureBindingF.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      val List(barLocal)       = barMethodBlock.astChildren.isLocal.name("f").l
      barLocal.closureBindingId shouldBe Option("Test0.swift:<global>.Foo.foo.bar:f")

      val List(identifierF) = barMethodBlock.ast.isIdentifier.nameExact("f").l
      identifierF.refOut.head shouldBe barLocal
    }

    "have correct structure for implicit self access except for variable in closure" in {
      val cpg = code("""
          |class Foo {
          |  let f: String = "f"
          |  class Bar {
          |    let b = "b"
          |    func foo() {
          |      let compare = { (s1: String, s2: String) -> Bool in
          |        let f: Int = 1
          |        handleB(b)
          |        handleF(f)
          |        return s1 > s2
          |	     }
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      cpg.typeDecl.nameExact("Bar").member.name.l shouldBe List("b")
      val List(bAccess) = cpg.call.nameExact("handleB").argument.fieldAccess.l
      bAccess.code shouldBe "self.b"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo.Bar"
        fieldName.canonicalName shouldBe "b"
      }
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.Int
        val fLocal = id._localViaRefOut.get
        fLocal.name shouldBe "f"
        fLocal.typeFullName shouldBe Defines.Int
      }

      val List(fooMethod)  = cpg.method.nameExact("foo").l
      val List(fooBlock)   = fooMethod.astChildren.isBlock.l
      val List(compareRef) = fooBlock.ast.isMethodRef.l
      compareRef.captureOut shouldBe empty

      val List(compareClosure)                              = cpg.method.nameExact("<lambda>0").l
      val List(compareClosureBlock)                         = compareClosure.astChildren.isBlock.l
      val List(compareClosureLocal)                         = compareClosureBlock.astChildren.isLocal.name("f").l
      val List(identifierFFromLocalDecl, identifierFInCall) = compareClosureBlock.ast.isIdentifier.nameExact("f").l
      identifierFFromLocalDecl.refOut.head shouldBe compareClosureLocal
      identifierFFromLocalDecl.lineNumber shouldBe Some(8)
      identifierFInCall.refOut.head shouldBe compareClosureLocal
      identifierFInCall.lineNumber shouldBe Some(10)
    }

    "have correct structure for implicit self access except for parameters" in {
      val cpg = code("""
          |class Foo {
          |  let f: String = "f"
          |  class Bar {
          |    let b = "b"
          |    func bar(f: Int) {
          |      handleB(b)
          |      handleF(f)
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      cpg.typeDecl.nameExact("Bar").member.name.l shouldBe List("b")
      val List(bAccess) = cpg.call.nameExact("handleB").argument.fieldAccess.l
      bAccess.code shouldBe "self.b"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo.Bar"
        fieldName.canonicalName shouldBe "b"
      }
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.Int
        inside(id.refsTo.l) { case List(p: MethodParameterIn) =>
          p.name shouldBe "f"
          p.typeFullName shouldBe Defines.Int
        }
      }
    }

    "have correct structure for implicit self access except for parameters in closure" in {
      val cpg = code("""
          |class Foo {
          |  let f: String = "f"
          |  class Bar {
          |    let b = "b"
          |    func bar() {
          |      let compare = { (f: String, g: String) -> Bool in
          |        handleB(b)
          |        handleF(f)
          |        return f > g
          |	     }
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").member.name.l shouldBe List("f")
      cpg.typeDecl.nameExact("Bar").member.name.l shouldBe List("b")
      val List(bAccess) = cpg.call.nameExact("handleB").argument.fieldAccess.l
      bAccess.code shouldBe "self.b"
      inside(bAccess.argument.l) { case List(id: Identifier, fieldName: FieldIdentifier) =>
        id.name shouldBe "self"
        id.typeFullName shouldBe "Test0.swift:<global>.Foo.Bar"
        fieldName.canonicalName shouldBe "b"
      }
      inside(cpg.call.nameExact("handleF").argument(1).l) { case List(id: Identifier) =>
        id.name shouldBe "f"
        id.typeFullName shouldBe Defines.String
        inside(id.refsTo.l) { case List(p: MethodParameterIn) =>
          p.name shouldBe "f"
          p.typeFullName shouldBe Defines.String
        }
      }
    }

    "have correct structure for function in accessor block" in {
      val cpg = code("""
          |public protocol Foo {
          |  var bar: Int {
          |    get {}
          |    set {}
          |    willSet {} // runs before bar is set
          |    didSet {} // runs after bar is set
          |  }
          |}
          |""".stripMargin)
      cpg.method.where(_._argumentIn) shouldBe empty

      val List(get, set, willSet, didSet) = cpg.typeDecl.nameExact("Foo").boundMethod.nameNot("init").l
      get.fullName shouldBe "Test0.swift:<global>.Foo.bar.getter:Swift.Int"

      set.fullName shouldBe "Test0.swift:<global>.Foo.bar.setter:Swift.Int"
      val List(self, setParam) = set.parameter.l
      self.name shouldBe "self"
      self.typeFullName shouldBe "Test0.swift:<global>.Foo"

      setParam.name shouldBe "newValue"
      setParam.typeFullName shouldBe "Swift.Int"

      willSet.fullName shouldBe "Test0.swift:<global>.Foo.bar.willSet:Swift.Int"
      didSet.fullName shouldBe "Test0.swift:<global>.Foo.bar.didSet:Swift.Int"
    }

    "have correct structure for function with variadic parameter" in {
      val cpg = code("""
          |func foo(a: String, b: String...) -> {}
          |""".stripMargin)
      val List(foo) = cpg.method.nameExact("foo").l
      foo.isVariadic shouldBe true
      val List(a, b) = foo.parameter.l
      a.name shouldBe "a"
      a.typeFullName shouldBe "Swift.String"
      b.name shouldBe "b"
      b.typeFullName shouldBe "Swift.String"
      b.isVariadic shouldBe true
    }

    "have correct structure for named call arguments" in {
      val cpg = code("""
          |func logMessage(message: String, prefix: String, suffix: String) {
          |  print("\(prefix)-\(message)-\(suffix)")
          |}
          |logMessage("error message", prefix: ">>>", suffix: "<<<")
          |""".stripMargin)
      inside(cpg.call.nameExact("logMessage").argument.l) { case List(_, message, prefix, suffix) =>
        message.code shouldBe "\"error message\""
        prefix.code shouldBe "\">>>\""
        prefix.argumentName shouldBe Some("prefix")
        suffix.code shouldBe "\"<<<\""
        suffix.argumentName shouldBe Some("suffix")
      }
    }
  }

}
