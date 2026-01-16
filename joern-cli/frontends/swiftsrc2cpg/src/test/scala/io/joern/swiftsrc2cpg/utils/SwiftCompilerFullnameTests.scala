package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class SwiftCompilerFullnameTests extends SwiftCompilerSrc2CpgSuite {

  "Using fullnames from the Swift compiler output" should {

    "be correct in a simple Hello World example" in {
      val cpg = codeWithSwiftSetup("""
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
      val List(mainConstructorCall) = cpg.call.nameExact("init").l
      mainConstructorCall.methodFullName shouldBe "SwiftTest.Main.init:()->SwiftTest.Main"

      mainConstructor.fullName shouldBe mainConstructorCall.methodFullName

      val List(printCall) = cpg.call.nameExact("print").l
      printCall.methodFullName shouldBe "Swift.print:(_:Any...,separator:Swift.String,terminator:Swift.String)->()"
      printCall.signature shouldBe "(_:Any...,separator:Swift.String,terminator:Swift.String)->()"
      printCall.typeFullName shouldBe "()"
      printCall.isStatic shouldBe true
    }

    "be correct for variable declarations" in {
      val cpg = codeWithSwiftSetup("""
          |let a = 1
          |let b = "b"
          |var c = 0.1
          |let d = 2, e = 3
          |let f: Int, g: Float
          |""".stripMargin)
      val Seq(a, b, c, d, e, f, g) = cpg.file(".+main.swift").ast.isLocal.sortBy(_.name)
      a.typeFullName shouldBe "Swift.Int"
      b.typeFullName shouldBe "Swift.String"
      c.typeFullName shouldBe "Swift.Double"
      d.typeFullName shouldBe "Swift.Int"
      e.typeFullName shouldBe "Swift.Int"
      f.typeFullName shouldBe "Swift.Int"
      g.typeFullName shouldBe "Swift.Float"

      val Seq(aId, bId, cId, dId, eId) = cpg.file(".+main.swift").ast.isIdentifier.sortBy(_.name)
      aId.typeFullName shouldBe "Swift.Int"
      bId.typeFullName shouldBe "Swift.String"
      cId.typeFullName shouldBe "Swift.Double"
      dId.typeFullName shouldBe "Swift.Int"
      eId.typeFullName shouldBe "Swift.Int"
    }

  }

}
