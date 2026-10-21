package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
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

    "use subject element types for tuple switch de-sugaring" in {
      val cpg = codeWithSwiftSetup("""
          |func f(x: Int) {
          |  switch (x, "s", (1.5, true)) {
          |    case let (a, b, (c, d)):
          |      print(a)
          |    default:
          |      break
          |  }
          |}
          |""".stripMargin)
      val tupleType = "(Swift.Int,Swift.String,(Swift.Double,Swift.Bool))"
      cpg.local.nameExact("<subject>0").typeFullName.loneElement shouldBe tupleType
      cpg.identifier.nameExact("<subject>0").typeFullName.dedup.loneElement shouldBe tupleType

      cpg.call.codeExact("a = <subject>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("b = <subject>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.String"
      cpg.call.codeExact("c = <subject>0.2.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Double"
      cpg.call.codeExact("d = <subject>0.2.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Bool"
      // the intermediate access of the nested tuple
      val cAccess = cpg.call.codeExact("c = <subject>0.2.0").argument(2).isCall
      cAccess.argument(1).isCall.typeFullName.loneElement shouldBe "(Swift.Double,Swift.Bool)"

      cpg.local.nameExact("a").typeFullName.loneElement shouldBe "Swift.Int"
      cpg.local.nameExact("b").typeFullName.loneElement shouldBe "Swift.String"
      cpg.local.nameExact("c").typeFullName.loneElement shouldBe "Swift.Double"
      cpg.local.nameExact("d").typeFullName.loneElement shouldBe "Swift.Bool"
    }

    "use pattern types for tuple switch elements when the subject is not a tuple literal" in {
      val cpg = codeWithSwiftSetup("""
          |func f(t: (Int, String)) {
          |  switch t {
          |    case (1, "a"):
          |      print("a")
          |    case let (a, b):
          |      print(a)
          |  }
          |}
          |""".stripMargin)
      cpg.local.nameExact("<subject>0").typeFullName.loneElement shouldBe "(Swift.Int,Swift.String)"
      // matching patterns: the literals
      cpg.call.codeExact("<subject>0.0 == 1").argument(1).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("<subject>0.1 == \"a\"").argument(1).isCall.typeFullName.loneElement shouldBe "Swift.String"
      // binding patterns
      cpg.call.codeExact("a = <subject>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("b = <subject>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.String"
      cpg.local.nameExact("a").typeFullName.loneElement shouldBe "Swift.Int"
      cpg.local.nameExact("b").typeFullName.loneElement shouldBe "Swift.String"
    }

    "use subject element types for tuple declarations" in {
      val cpg = codeWithSwiftSetup("""
          |func f(x: Int) {
          |  let (a, b) = (x, "s")
          |}
          |""".stripMargin)
      cpg.local.nameExact("<tmp>0").typeFullName.loneElement shouldBe "(Swift.Int,Swift.String)"
      cpg.identifier.nameExact("<tmp>0").typeFullName.dedup.loneElement shouldBe "(Swift.Int,Swift.String)"
      cpg.call.codeExact("a = <tmp>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("b = <tmp>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.String"
      cpg.local.nameExact("a").typeFullName.loneElement shouldBe "Swift.Int"
      cpg.local.nameExact("b").typeFullName.loneElement shouldBe "Swift.String"
    }

    "use subject element types for tuple if-case" in {
      val cpg = codeWithSwiftSetup("""
          |func f(x: Int) {
          |  if case (let a, let b) = (x, "s") {
          |    print("x")
          |  }
          |}
          |""".stripMargin)
      cpg.local.nameExact("<tmp>0").typeFullName.loneElement shouldBe "(Swift.Int,Swift.String)"
      cpg.call.codeExact("a = <tmp>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("b = <tmp>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.String"
      cpg.local.nameExact("a").typeFullName.loneElement shouldBe "Swift.Int"
      cpg.local.nameExact("b").typeFullName.loneElement shouldBe "Swift.String"
    }

    "use subject element types for tuple guard-case" in {
      val cpg = codeWithSwiftSetup("""
          |func f(x: Int) {
          |  guard case (let c, let d) = (x, 1.5) else { return }
          |}
          |""".stripMargin)
      cpg.local.nameExact("<tmp>0").typeFullName.loneElement shouldBe "(Swift.Int,Swift.Double)"
      cpg.call.codeExact("c = <tmp>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("d = <tmp>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Double"
      cpg.local.nameExact("c").typeFullName.loneElement shouldBe "Swift.Int"
      cpg.local.nameExact("d").typeFullName.loneElement shouldBe "Swift.Double"
    }

    "not mix up subject temps with the same name in nested methods" in {
      val cpg = codeWithSwiftSetup("""
          |func f(x: Int) {
          |  switch (x, "s") {
          |    case (1, _):
          |      func g() {
          |        switch (1.5, true) {
          |          case let (c, d): print(c, d)
          |        }
          |      }
          |    case let (a, b): print(a, b)
          |  }
          |}
          |""".stripMargin)
      cpg.call.codeExact("a = <subject>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Int"
      cpg.call.codeExact("b = <subject>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.String"
      cpg.call.codeExact("c = <subject>0.0").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Double"
      cpg.call.codeExact("d = <subject>0.1").argument(2).isCall.typeFullName.loneElement shouldBe "Swift.Bool"
    }

  }

}
