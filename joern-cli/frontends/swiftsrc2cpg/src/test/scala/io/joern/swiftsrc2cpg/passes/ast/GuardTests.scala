// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GuardTests extends AbstractPassTest {

  "GuardTests" should {

    "testGuard1" ignore AstFixture("""
        |func noConditionNoElse() {
        |  guard {} else {}
        |}
        |""".stripMargin) { cpg => }

    "testGuard2" ignore AstFixture("""
        |var i = 2
        |while (i <= 10) {
        |  guard i % 2 == 0 else {
        |    i = i + 1
        |    continue
        |  }
        |  print(i)
        |  i = i + 1
        |} 
        |""".stripMargin) { cpg => }

    "testGuard3" ignore AstFixture("""
        |func checkOddEven() {
        |  var number = 24
        |  guard number % 2 == 0 else {
        |    print("Odd Number")
        |    return
        |  }
        |  print("Even Number")
        |}
        |""".stripMargin) { cpg => }

    "testGuard4" ignore AstFixture("""
        |func checkJobEligibility() {
        |  var age = 33
        |  guard age >= 18, age <= 40 else {
        |    print("Not Eligible for Job")
        |    return
        |  }
        |  print("You are eligible for this job")
        |}
        |""".stripMargin) { cpg => }

    "testGuard5" ignore AstFixture("""
        |func checkAge() {
        |  var age: Int? = 22
        |  guard let myAge = age else {
        |    print("Age is undefined")
        |    return
        |  }
        |  print("My age is \(myAge)")
        |}
        |""".stripMargin) { cpg => }

  }

}
