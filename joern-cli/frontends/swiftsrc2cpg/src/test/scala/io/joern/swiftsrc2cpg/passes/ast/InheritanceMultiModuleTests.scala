package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerMultiModuleSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class InheritanceMultiModuleTests extends SwiftCompilerMultiModuleSrc2CpgSuite {

  private val mainCode = """
     |import ModuleA
     |import ModuleB
     |
     |class Main: A, B {}
     |""".stripMargin

  private val moduleACode =
    """
      |open class A {}
      |""".stripMargin

  private val moduleBCode =
    """
      |public protocol B {}
      |""".stripMargin

  private val cpg =
    codeWithSwiftSetup(mainCode, "SwiftTest/Sources/SwiftTest/main.swift")
      .moreCode(moduleACode, "SwiftTest/Sources/ModuleA/A.swift")
      .moreCode(moduleBCode, "SwiftTest/Sources/ModuleB/B.swift")

  "InheritanceMultiModuleTests" should {

    "create correct inheritance type decl fullNames" in {
      val List(mainTypeDecl) = cpg.typeDecl.nameExact("Main").l
      mainTypeDecl.fullName shouldBe "SwiftTest.Main"

      mainTypeDecl.inheritsFromTypeFullName.l should contain allElementsOf List("ModuleA.A", "ModuleB.B")

      val List(aTypeDecl) = cpg.typeDecl.nameExact("A").l
      aTypeDecl.fullName shouldBe "ModuleA.A"

      val List(bTypeDecl) = cpg.typeDecl.nameExact("B").l
      bTypeDecl.fullName shouldBe "ModuleB.B"
    }

  }

}
