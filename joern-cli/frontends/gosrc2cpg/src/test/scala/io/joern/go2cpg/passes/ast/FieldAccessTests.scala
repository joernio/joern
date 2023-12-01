package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends GoCodeToCpgSuite {

  "simple field access test" should {
    val cpg = code("""
        |package main
        |type Person struct {
        |	fname string
        |	lname string
        |}
        |func (person Person) fullName() string {
        |	return person.fname + " " + person.lname
        |}
        |func main() {
        |	var a Person = Person{fname: "Pandurang", lname: "Patil"}
        |	var fn string = a.fname
        |}
        |
        |func foo() {
        |   var a Person = Person{fname: "Pandurang", lname: "Patil"}
        |   println(a.fname)
        |}
        |""".stripMargin)

    "External Field access test" in {
      val List(fieldAccessCall) = cpg.method("main").astChildren.fieldAccess.l
      fieldAccessCall.name shouldBe Operators.fieldAccess
      fieldAccessCall.typeFullName shouldBe "string"

      val List(ident: Identifier, fieldIdent: FieldIdentifier) = fieldAccessCall.argument.l: @unchecked
      ident.name shouldBe "a"
      fieldIdent.canonicalName shouldBe "fname"
    }

    "Field access test for internal function of struct" in {
      val List(facFname, facLname) = cpg.method("fullName").astChildren.fieldAccess.l
      facFname.name shouldBe Operators.fieldAccess
      facLname.name shouldBe Operators.fieldAccess
    }

    "Field access test while passing argument to method call" in {
      val List(fieldAccessCall: Call) = cpg.call("println").argument.l: @unchecked
      fieldAccessCall.name shouldBe Operators.fieldAccess
    }
  }
}

//TODO: Add unit test for global public variable defined in one package and getting accessed in another package.
// Its construct will be similar to fieldAccess
