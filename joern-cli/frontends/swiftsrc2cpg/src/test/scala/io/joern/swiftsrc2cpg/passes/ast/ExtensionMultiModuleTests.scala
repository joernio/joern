package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerMultiModuleSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ExtensionMultiModuleTests extends SwiftCompilerMultiModuleSrc2CpgSuite {

  private val mainCode = """
     |import ModuleA
     |import ModuleB
     |
     |func main() {
     |    Foo().foo()
     |    Foo().bar()
     |}
     |""".stripMargin

  private val moduleACode =
    """
      |public class Foo {
      |  public var x = 0
      |
      |  public init() {}
      |}
      |
      |public extension Foo {
      |  var a: Int { return 1 }
      |
      |  func foo() {}
      |}
      |""".stripMargin

  private val moduleBCode =
    """
      |import ModuleA
      |
      |public extension Foo {
      |  var b: String { return "hello" }
      |
      |  func bar() {
      |    print(x)
      |  }
      |}
      |""".stripMargin

  private val cpg =
    codeWithSwiftSetup(mainCode, "SwiftTest/Sources/SwiftTest/main.swift")
      .moreCode(moduleACode, "SwiftTest/Sources/ModuleA/Foo.swift")
      .moreCode(moduleBCode, "SwiftTest/Sources/ModuleB/FooExt.swift")

  "ExtensionMultiModuleTests" should {

    "create correct extension method fullNames" in {
      val List(fooTypeDecl) = cpg.typeDecl.nameExact("Foo").l
      fooTypeDecl.fullName shouldBe "ModuleA.Foo"

      val List(foo) = cpg.method.nameExact("foo").l
      // The extension method foo for class Foo is defined in the same module as Foo itself.
      // Hence, the compiler does not prefix the fullName with ModuleA.
      foo.fullName shouldBe "ModuleA.Foo<extension>.foo:()->()"

      val List(bar) = cpg.method.nameExact("bar").l
      // The extension method bar for class Foo is defined in a different module than Foo itself.
      // Hence, the compiler does prefix the fullName with ModuleB (where the extension is defined).
      bar.fullName shouldBe "ModuleB.ModuleA.Foo<extension>.bar:()->()"

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe "ModuleA.Foo<extension>.foo:()->()"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe "ModuleB.ModuleA.Foo<extension>.bar:()->()"

      val List(selfParamInBar) = cpg.method.nameExact("bar").parameter.nameExact("self").l
      selfParamInBar.typeFullName shouldBe "ModuleA.Foo"

      val List(printCall)  = cpg.call.nameExact("print").l
      val List(selfAccess) = printCall.argument.isCall.l
      selfAccess.code shouldBe "self.x"
      selfAccess.typeFullName shouldBe "Swift.Int"
      val List(selfIdentifier) = selfAccess.argument.isIdentifier.l
      selfIdentifier.name shouldBe "self"
      selfIdentifier.typeFullName shouldBe "ModuleA.Foo"

      val Seq(a, b, x) = fooTypeDecl.member.sortBy(_.name)
      a.name shouldBe "a"
      b.name shouldBe "b"
      x.name shouldBe "x"
      a.typeFullName shouldBe "Swift.Int"
      b.typeFullName shouldBe "Swift.String"
      x.typeFullName shouldBe "Swift.Int"
    }

  }

}
