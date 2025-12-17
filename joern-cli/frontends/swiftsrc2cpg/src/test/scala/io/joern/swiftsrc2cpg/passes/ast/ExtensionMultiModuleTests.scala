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
      |    public init() {}
      |}
      |
      |public extension Foo {
      |  func foo() {}
      |}
      |""".stripMargin

  private val moduleBCode =
    """
      |import ModuleA
      |
      |public extension Foo {
      |  func bar() {}
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
    }

  }

}
