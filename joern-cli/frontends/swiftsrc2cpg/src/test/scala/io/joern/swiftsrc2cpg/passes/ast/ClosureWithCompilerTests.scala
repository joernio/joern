package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ClosureWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "ClosureWithCompilerTests" should {

    "create type decls and bindings correctly (local closure declaration)" in {
      // We test all closures here in one example because running the Swift compiler
      // on Windows machines tend to be quite slow. Splitting them into individual test cases
      // just results on awfully long test-code-test-repeat cycles.
      val testCode =
        """
          |func main() {
          |  // closure call is allways "function_ref": "single_apply"
          |
          |	 let compare = { (s1: String, s2: String) -> Bool in
          |    return s1 > s2
          |	 }
          |	 let compareResult = compare("1", "2")
          |
          |	 var customersInLine = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]
          |	 // auto-closure example 1:
          |	 let customerProvider = { customersInLine.remove(at: 0) }
          |	 let customerProviderResult = customerProvider()
          |
          |	 // auto-closure example 2:
          |	 let greet = {
          |    print("Hello, World!")
          |	 }
          |	 greet()
          |
          |	 let greetUser = { (name: String) in
          |	   print("Hey there, \(name).")
          |	 }
          |	 greetUser("Alex")
          |
          |	 let findSquare = { (num: Int) -> (Int) in
          |    let square = num * num
          |	   return square
          |	 }
          |	 let findSquareResult = findSquare(5)
          |}""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)

      // compare ((Swift.String,Swift.String)->Swift.Bool)
      val List(compareLocal) = cpg.local.nameExact("compare").l
      compareLocal.typeFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"

      val List(compareResultLocal) = cpg.local.nameExact("compareResult").l
      compareResultLocal.typeFullName shouldBe "Swift.Bool"

      val compareClosureFullName = "Sources/main.swift:<global>.main.<lambda>0:(Swift.String,Swift.String)->Swift.Bool"
      val List(compareClosure)   = cpg.method.fullNameExact(compareClosureFullName).l
      val List(compareClosureTypeDecl) = cpg.typeDecl.fullNameExact(compareClosureFullName).l
      compareClosureTypeDecl.inheritsFromTypeFullName.l shouldBe List(
        "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      )
      val List(compareClosureBinding) = compareClosureTypeDecl.bindsOut.l
      compareClosureBinding.name shouldBe "single_apply"
      compareClosureBinding.methodFullName shouldBe compareClosureFullName
      compareClosureBinding.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"

      val List(compareClosureCall) = cpg.call.codeExact("""compare("1", "2")""").l
      compareClosureCall.name shouldBe "single_apply"
      compareClosure.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"
      compareClosureCall.methodFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>.single_apply:(Swift.String,Swift.String)->Swift.Bool"
      compareClosureCall.receiver.isIdentifier.name.l shouldBe List("compare")
      compareClosureCall.receiver.isIdentifier.typeFullName.l shouldBe List(
        "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      )
      compareClosureCall.argument(1).code shouldBe """"1""""
      compareClosureCall.argument(2).code shouldBe """"2""""

      // customerProvider (no-arg -> Swift.String)
      val List(customerProviderLocal) = cpg.local.nameExact("customerProvider").l
      customerProviderLocal.typeFullName shouldBe "Swift.Function<()->Swift.String>"

      val List(customerProviderResultLocal) = cpg.local.nameExact("customerProviderResult").l
      customerProviderResultLocal.typeFullName shouldBe "Swift.String"

      val customerProviderClosureFullName = "Sources/main.swift:<global>.main.<lambda>1:()->Swift.String"
      val List(customerProviderClosure)   = cpg.method.fullNameExact(customerProviderClosureFullName).l
      val List(customerProviderTypeDecl)  = cpg.typeDecl.fullNameExact(customerProviderClosureFullName).l
      customerProviderTypeDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Function<()->Swift.String>")
      val List(customerProviderBinding) = customerProviderTypeDecl.bindsOut.l
      customerProviderBinding.name shouldBe "single_apply"
      customerProviderBinding.methodFullName shouldBe customerProviderClosureFullName
      customerProviderBinding.signature shouldBe "()->Swift.String"

      val List(customerProviderCall) = cpg.call.codeExact("""customerProvider()""").l
      customerProviderCall.name shouldBe "single_apply"
      customerProviderClosure.signature shouldBe "()->Swift.String"
      customerProviderCall.methodFullName shouldBe "Swift.Function<()->Swift.String>.single_apply:()->Swift.String"
      customerProviderCall.receiver.isIdentifier.name.l shouldBe List("customerProvider")
      customerProviderCall.receiver.isIdentifier.typeFullName.l shouldBe List("Swift.Function<()->Swift.String>")

      // greet (no-arg -> ())
      val List(greetLocal) = cpg.local.nameExact("greet").l
      greetLocal.typeFullName shouldBe "Swift.Function<()->()>"

      val greetClosureFullName = "Sources/main.swift:<global>.main.<lambda>2:()->()"
      val List(greetClosure)   = cpg.method.fullNameExact(greetClosureFullName).l
      val List(greetTypeDecl)  = cpg.typeDecl.fullNameExact(greetClosureFullName).l
      greetTypeDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Function<()->()>")
      val List(greetBinding) = greetTypeDecl.bindsOut.l
      greetBinding.name shouldBe "single_apply"
      greetBinding.methodFullName shouldBe greetClosureFullName
      greetBinding.signature shouldBe "()->()"

      val List(greetCall) = cpg.call.codeExact("""greet()""").l
      greetCall.name shouldBe "single_apply"
      greetClosure.signature shouldBe "()->()"
      greetCall.methodFullName shouldBe "Swift.Function<()->()>.single_apply:()->()"
      greetCall.receiver.isIdentifier.name.l shouldBe List("greet")
      greetCall.receiver.isIdentifier.typeFullName.l shouldBe List("Swift.Function<()->()>")

      // greetUser (Swift.String -> ())
      val List(greetUserLocal) = cpg.local.nameExact("greetUser").l
      greetUserLocal.typeFullName shouldBe "Swift.Function<(Swift.String)->()>"

      val greetUserClosureFullName = "Sources/main.swift:<global>.main.<lambda>3:(Swift.String)->()"
      val List(greetUserClosure)   = cpg.method.fullNameExact(greetUserClosureFullName).l
      val List(greetUserTypeDecl)  = cpg.typeDecl.fullNameExact(greetUserClosureFullName).l
      greetUserTypeDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Function<(Swift.String)->()>")
      val List(greetUserBinding) = greetUserTypeDecl.bindsOut.l
      greetUserBinding.name shouldBe "single_apply"
      greetUserBinding.methodFullName shouldBe greetUserClosureFullName
      greetUserBinding.signature shouldBe "(Swift.String)->()"

      val List(greetUserCall) = cpg.call.codeExact("""greetUser("Alex")""").l
      greetUserCall.name shouldBe "single_apply"
      greetUserClosure.signature shouldBe "(Swift.String)->()"
      greetUserCall.methodFullName shouldBe "Swift.Function<(Swift.String)->()>.single_apply:(Swift.String)->()"
      greetUserCall.receiver.isIdentifier.name.l shouldBe List("greetUser")
      greetUserCall.receiver.isIdentifier.typeFullName.l shouldBe List("Swift.Function<(Swift.String)->()>")
      greetUserCall.argument(1).code shouldBe """"Alex""""

      // findSquare (Swift.Int -> Swift.Int)
      val List(findSquareLocal) = cpg.local.nameExact("findSquare").l
      findSquareLocal.typeFullName shouldBe "Swift.Function<(Swift.Int)->Swift.Int>"

      val List(findSquareResultLocal) = cpg.local.nameExact("findSquareResult").l
      findSquareResultLocal.typeFullName shouldBe "Swift.Int"

      val findSquareClosureFullName = "Sources/main.swift:<global>.main.<lambda>4:(Swift.Int)->Swift.Int"
      val List(findSquareClosure)   = cpg.method.fullNameExact(findSquareClosureFullName).l
      val List(findSquareTypeDecl)  = cpg.typeDecl.fullNameExact(findSquareClosureFullName).l
      findSquareTypeDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Function<(Swift.Int)->Swift.Int>")
      val List(findSquareBinding) = findSquareTypeDecl.bindsOut.l
      findSquareBinding.name shouldBe "single_apply"
      findSquareBinding.methodFullName shouldBe findSquareClosureFullName
      findSquareBinding.signature shouldBe "(Swift.Int)->Swift.Int"

      val List(findSquareCall) = cpg.call.codeExact("""findSquare(5)""").l
      findSquareCall.name shouldBe "single_apply"
      findSquareClosure.signature shouldBe "(Swift.Int)->Swift.Int"
      findSquareCall.methodFullName shouldBe "Swift.Function<(Swift.Int)->Swift.Int>.single_apply:(Swift.Int)->Swift.Int"
      findSquareCall.receiver.isIdentifier.name.l shouldBe List("findSquare")
      findSquareCall.receiver.isIdentifier.typeFullName.l shouldBe List("Swift.Function<(Swift.Int)->Swift.Int>")
      findSquareCall.argument(1).code shouldBe "5"
    }

    "create type decls and bindings correctly (class variable closure declaration)" in {
      val testCode =
        """
          |class Foo {
          |  var compare = { (s1: String, s2: String) -> Bool in
          |    return s1 > s2
          |	 }
          |
          |  func main() {
          |	   let compareResult = compare("1", "2")
          |  }
          |}
          |Foo().main()
          |""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)
      val compareClosureFullName =
        "Sources/main.swift:<global>.Foo.init.<lambda>0:(Swift.String,Swift.String)->Swift.Bool"

      cpg.local.nameExact("compare") shouldBe empty

      val List(fooConstructor)    = cpg.method.isConstructor.fullNameExact("SwiftTest.Foo.init:()->SwiftTest.Foo").l
      val List(compareAssignment) = fooConstructor.ast.isCall.isAssignment.l
      val List(compareTarget)     = compareAssignment.target.fieldAccess.l
      compareTarget.code shouldBe "self.compare"
      compareTarget.typeFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      inside(compareTarget.argument.l) { case List(selfId: Identifier, fieldId: FieldIdentifier) =>
        selfId.typeFullName shouldBe "SwiftTest.Foo"
        fieldId.code shouldBe "compare"
      }

      val compareSource = compareAssignment.source.asInstanceOf[MethodRef]
      compareSource.methodFullName shouldBe compareClosureFullName

      val List(compareResultLocal) = cpg.local.nameExact("compareResult").l
      compareResultLocal.typeFullName shouldBe "Swift.Bool"

      val List(compareClosure)         = cpg.method.fullNameExact(compareClosureFullName).l
      val List(compareClosureTypeDecl) = cpg.typeDecl.fullNameExact(compareClosureFullName).l
      compareClosureTypeDecl.inheritsFromTypeFullName.l shouldBe List(
        "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      )
      val List(compareClosureBinding) = compareClosureTypeDecl.bindsOut.l
      compareClosureBinding.name shouldBe "single_apply"
      compareClosureBinding.methodFullName shouldBe compareClosureFullName
      compareClosureBinding.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"

      val List(compareClosureCall) = cpg.call.codeExact("""compare("1", "2")""").l
      compareClosureCall.name shouldBe "single_apply"
      compareClosure.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"
      compareClosureCall.methodFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>.single_apply:(Swift.String,Swift.String)->Swift.Bool"

      val List(compareClosureCallReceiver) = compareClosureCall.receiver.fieldAccess.l
      compareClosureCallReceiver.code shouldBe "self.compare"
      compareClosureCallReceiver.typeFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      inside(compareClosureCallReceiver.argument.l) { case List(selfId: Identifier, fieldId: FieldIdentifier) =>
        selfId.typeFullName shouldBe "SwiftTest.Foo"
        fieldId.code shouldBe "compare"
      }
      compareClosureCall.argument(1).code shouldBe """"1""""
      compareClosureCall.argument(2).code shouldBe """"2""""

      val List(mainCall) = cpg.call.nameExact("main").l
      mainCall.methodFullName shouldBe "SwiftTest.Foo.main:()->()"
      mainCall.signature shouldBe "()->()"
      mainCall.typeFullName shouldBe "()"
      val List(fooConstructorCall) = mainCall.receiver.isBlock.astChildren.isCall.nameExact("init").l
      fooConstructorCall.methodFullName shouldBe "SwiftTest.Foo.init:()->SwiftTest.Foo"
      fooConstructorCall.signature shouldBe "()->SwiftTest.Foo"
      fooConstructorCall.typeFullName shouldBe "SwiftTest.Foo"
      fooConstructorCall._methodViaCallOut.l shouldBe List(fooConstructor)
    }

    "create type decls and bindings correctly (closure as function parameter)" in {
      val testCode =
        """
          |func runCompare(compare:(String, String) -> Bool) {
          |	 let compareResult = compare("1", "2")
          |}
          |
          |func main() {
          |  let compareFunc = { (s1: String, s2: String) -> Bool in
          |    return s1 > s2
          |	 }
          |  runCompare(compare: compareFunc)
          |}""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)

      val List(runCompareCall) = cpg.call.nameExact("runCompare").l
      runCompareCall.methodFullName shouldBe "SwiftTest.runCompare:(compare:(Swift.String,Swift.String)->Swift.Bool)->()"

      val List(runCompareMethod) = cpg.method.nameExact("runCompare").l
      runCompareMethod.fullName shouldBe "SwiftTest.runCompare:(compare:(Swift.String,Swift.String)->Swift.Bool)->()"

      val List(compareFuncLocal) = cpg.local.nameExact("compareFunc").l
      compareFuncLocal.typeFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"

      val List(compareResultLocal) = cpg.local.nameExact("compareResult").l
      compareResultLocal.typeFullName shouldBe "Swift.Bool"

      val compareClosureFullName = "Sources/main.swift:<global>.main.<lambda>0:(Swift.String,Swift.String)->Swift.Bool"
      val List(compareClosure)   = cpg.method.fullNameExact(compareClosureFullName).l
      val List(compareClosureTypeDecl) = cpg.typeDecl.fullNameExact(compareClosureFullName).l
      compareClosureTypeDecl.inheritsFromTypeFullName.l shouldBe List(
        "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      )
      val List(compareClosureBinding) = compareClosureTypeDecl.bindsOut.l
      compareClosureBinding.name shouldBe "single_apply"
      compareClosureBinding.methodFullName shouldBe compareClosureFullName
      compareClosureBinding.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"

      val List(compareClosureCall) = cpg.call.codeExact("""compare("1", "2")""").l
      compareClosureCall.name shouldBe "single_apply"
      compareClosure.signature shouldBe "(Swift.String,Swift.String)->Swift.Bool"
      compareClosureCall.methodFullName shouldBe "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>.single_apply:(Swift.String,Swift.String)->Swift.Bool"
      compareClosureCall.receiver.isIdentifier.name.l shouldBe List("compare")
      compareClosureCall.receiver.isIdentifier.typeFullName.l shouldBe List(
        "Swift.Function<(Swift.String,Swift.String)->Swift.Bool>"
      )
      compareClosureCall.argument(1).code shouldBe """"1""""
      compareClosureCall.argument(2).code shouldBe """"2""""
    }

  }

}
