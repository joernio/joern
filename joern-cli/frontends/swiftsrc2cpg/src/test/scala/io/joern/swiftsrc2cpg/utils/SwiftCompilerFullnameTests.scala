package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class SwiftCompilerFullnameTests extends AstSwiftCompilerSrc2CpgSuite {

  private def codeWithSwiftSetup(codeString: String) = {
    code(codeString, "SwiftTest/Sources/main.swift")
      .moreCode(
        """
        |// swift-tools-version: 6.1
        |// The swift-tools-version declares the minimum version of Swift required to build this package.
        |import PackageDescription
        |let package = Package(
        |    name: "SwiftTest",
        |    targets: [
        |        .executableTarget(name: "SwiftTest"),
        |    ]
        |)
        |""".stripMargin,
        "SwiftTest/Package.swift"
      )
  }

  "Using fullnames from the Swift compiler output" should {

    "be correct in a simple Hello World example" in {
      val cpg = codeWithSwiftSetup("""
          |import Foundation
          |
          |class Main {
          |  func hello() {
          |    print("Hello World!")
          |  }
          |}
          |
          |let main = Main()
          |main.hello()
          |""".stripMargin)
      val List(helloMethod) = cpg.method.nameExact("hello").l
      helloMethod.fullName shouldBe "SwiftTest.Main.hello:()->()"
      val List(helloCall) = cpg.call.nameExact("hello").l
      helloCall.methodFullName shouldBe "SwiftTest.Main.hello:()->()"

      helloMethod.fullName shouldBe helloCall.methodFullName

      val List(mainTypeDecl) = cpg.typeDecl.nameExact("Main").l
      mainTypeDecl.fullName shouldBe "SwiftTest.Main"
      val List(mainConstructor) = mainTypeDecl.ast.isMethod.isConstructor.l
      mainConstructor.fullName shouldBe "SwiftTest.Main.init:()->SwiftTest.Main"
      val List(mainConstructorCall) = cpg.call.nameExact("Main").l
      mainConstructorCall.methodFullName shouldBe "SwiftTest.Main.init:()->SwiftTest.Main"

      mainConstructor.fullName shouldBe mainConstructorCall.methodFullName

      val List(printCall) = cpg.call.nameExact("print").l
      printCall.methodFullName shouldBe "Swift.print:(_:Any...,separator:Swift.String,terminator:Swift.String)->()"
      printCall.signature shouldBe "(_:Any...,separator:Swift.String,terminator:Swift.String)->()"
      printCall.typeFullName shouldBe "()"
      printCall.isStatic shouldBe true
    }

  }

}
