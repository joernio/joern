package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, nodes}
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

    "check this/receiver argument is correctly getting passed" in {
      cpg.call("fullName").argument.size shouldBe 1
      val List(x: Identifier) = cpg.call("fullName").argument.l: @unchecked
      x.order shouldBe 1
      x.argumentIndex shouldBe 0
      x.name shouldBe "a"
    }
  }
  "Method call on chain of variable" should {
    "Check call node properties on two times chained" in {
      val cpg = code("""
        package main
          |
          |func foo(ctx context) int {
          |
          |   var a = 10
          |	result := ctx.data.bar(a)
          |
          |	return result
          |}
          |
          |""".stripMargin)

      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "ctx.data.bar(a)"
      x.methodFullName shouldBe "ANY.bar"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe Defines.anyTypeName
    }

    "Check call node properties on five times chained" in {
      val cpg = code("""
        package main
          |
          |func foo(ctx context) int {
          |
          | var a = 10
          |	result := ctx.data1.data2.data3.data4.bar(a)
          |
          |	return result
          |}
          |
          |""".stripMargin)

      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "ctx.data1.data2.data3.data4.bar(a)"
      x.methodFullName shouldBe "ANY.bar"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe Defines.anyTypeName
    }

  }

  "Method call chaining using builder design" should {
    val cpg = code("""
        |package main
        |type Person struct {
        |    name     string
        |    age      int
        |    location string
        |}
        |func (p *Person) SetName(name string) *Person {
        |    p.name = name
        |    return p
        |}
        |func (p *Person) SetAge(age int) *Person {
        |    p.age = age
        |    return p
        |}
        |func (p *Person) SetLocation(location string) *Person {
        |    p.location = location
        |    return p
        |}
        |func main() {
        |    var person *Person = &Person{}
        |    person.SetName("John").SetAge(30).SetLocation("New York")
        |}
        |""".stripMargin)

    "Check call node properties: Name" in {
      val List(x) = cpg.call.name("SetName").l
      x.code shouldBe "person.SetName(\"John\")"
      x.methodFullName shouldBe "main.Person.SetName"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(22)
      x.typeFullName shouldBe "*main.Person"

      val List(receiver: Identifier, _) = x.argument.l: @unchecked
      receiver.name shouldBe "person"
    }

    "Check call node properties: Age" in {
      val List(x) = cpg.call.name("SetAge").l
      x.code shouldBe "person.SetName(\"John\").SetAge(30)"
      x.methodFullName shouldBe "main.Person.SetAge"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(22)
      x.typeFullName shouldBe "*main.Person"

      val List(receiver: Call, _) = x.argument.l: @unchecked
      receiver.name shouldBe "SetName"
    }

    "Check call node properties: Location" in {
      val List(x) = cpg.call.name("SetLocation").l
      x.code shouldBe "person.SetName(\"John\").SetAge(30).SetLocation(\"New York\")"
      x.methodFullName shouldBe "main.Person.SetLocation"
      x.order shouldBe 3
      x.lineNumber shouldBe Option(22)
      x.typeFullName shouldBe "*main.Person"

      val List(receiver: Call, _) = x.argument.l: @unchecked
      receiver.name shouldBe "SetAge"
    }
  }
}
