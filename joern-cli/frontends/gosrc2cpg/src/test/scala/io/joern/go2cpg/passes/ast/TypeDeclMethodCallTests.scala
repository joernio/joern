package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class TypeDeclMethodCallTests extends GoCodeToCpgSuite {

  "basic method call test on struct type object test" should {
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
        |	var fulname string = a.fullName()
        |}
        |""".stripMargin)

    "Check method node properties" in {
      cpg.method("fullName").size shouldBe 1
      val List(x) = cpg.method("fullName").l
      x.name shouldBe "fullName"
      x.fullName shouldBe "main.Person.fullName"
      x.code should startWith("func (person Person) fullName() string {")
      x.signature shouldBe "main.Person.fullName()string"
      x.isExternal shouldBe false
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
    }

    "Check call node properties" in {
      cpg.call("fullName").size shouldBe 1
      val List(x) = cpg.call("fullName").l
      x.code shouldBe "a.fullName()"
      x.methodFullName shouldBe "main.Person.fullName"
      x.signature shouldBe "main.Person.fullName()string"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(12)
      x.typeFullName shouldBe "string"
      x.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
