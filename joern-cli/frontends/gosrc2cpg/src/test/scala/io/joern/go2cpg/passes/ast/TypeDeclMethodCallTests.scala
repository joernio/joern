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

  "structure initialization having array of complex(structure-array) type" should {

    val cpg = code("""
        |package main
        |
        |type Phone struct {
        |	phone     string
        |	phonetype string
        |}
        |type Person struct {
        |	phone []Phone
        |	name  string
        |}
        |
        |func main() {
        |   numbers := [5]int{1, 2, 3, 4, 5}
        |	var fphone = []Phone{{phone: "1234567890", phonetype: "Home"}, {phone: "1234567890", phonetype: "Home"}}
        |	var person = Person{fphone, "Peter"}
        |}
        |""".stripMargin)

    "Check member nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Person").l
      typeDeclNode.member.size shouldBe 2

      val List(phone, name) = typeDeclNode.member.l
      phone.typeFullName shouldBe "[]main.Phone"
      name.typeFullName shouldBe "string"
    }

    "check literal nodes" in {
      val literalNodes = cpg.literal.l
      literalNodes.code("\"1234567890\"").size shouldBe 2
      literalNodes.code("\"Home\"").size shouldBe 2
      literalNodes.code("\"Peter\"").size shouldBe 1
    }

    "check arrayInitializer nodes" in {
      val List(intArrayNode, phoneArrayNode) = cpg.call(".*arrayInitializer").l
      intArrayNode.code shouldBe "[5]int{1, 2, 3, 4, 5}"
      phoneArrayNode.code shouldBe "[]Phone{{phone: \"1234567890\", phonetype: \"Home\"}, {phone: \"1234567890\", phonetype: \"Home\"}}"
    }
  }

  "structure initialization having array of int type" should {

    val cpg = code("""
        |package main
        |
        |import "fmt"
        |
        |type Person struct {
        |	phone []int
        |	name  string
        |}
        |
        |func main() {
        |	numbers := []int{1, 2, 3, 4, 5}
        |	var person = Person{numbers, "Peter"}
        |	fmt.Println(person)
        |
        |}
        |""".stripMargin)

    "Check member nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Person").l
      typeDeclNode.member.size shouldBe 2

      val List(phone, name) = typeDeclNode.member.l
      phone.typeFullName shouldBe "[]int"
      name.typeFullName shouldBe "string"
    }

    "check literal nodes" in {
      val literalNodes = cpg.literal.l
      literalNodes.code("1|2|3|4|5").size shouldBe 5
      literalNodes.code("\"Peter\"").size shouldBe 1
    }

    "check arrayInitializer nodes" in {
      val List(intArrayNode) = cpg.call(".*arrayInitializer").l
      intArrayNode.code shouldBe "[]int{1, 2, 3, 4, 5}"
    }
  }

  "structure initialization having array of complex(structure) type" should {

    val cpg = code("""
        package main
        |
        |import "fmt"
        |
        |type Address struct {
        |    Street  string
        |    City    string
        |    Country string
        |}
        |
        |type Person struct {
        |    Name    string
        |    Age     int
        |    Address Address
        |}
        |
        |func main() {
        |    person := Person{
        |        Name: "John Doe",
        |        Age:  30,
        |        Address: Address{
        |            Street:  "123 Main St",
        |            City:    "New York",
        |            Country: "USA",
        |        },
        |    }
        |}
        |""".stripMargin)

    "Check member nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Person").l
      typeDeclNode.member.size shouldBe 3

      val List(name, age, address) = typeDeclNode.member.l
      name.typeFullName shouldBe "string"
      age.typeFullName shouldBe "int"
      address.typeFullName shouldBe "main.Address"
    }

    "Check literal nodes" in {
      val literals = cpg.literal.l
      literals.code("\"John Doe\"").size shouldBe 1
      literals.code("\"123 Main St\"").size shouldBe 1
      literals.code("\"New York\"").size shouldBe 1
      literals.code("\"John Doe\"").size shouldBe 1
      literals.code("\"USA\"").size shouldBe 1
      literals.code("30").size shouldBe 1
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
      x.methodFullName shouldBe "main.context.data.<FieldAccess>.<unknown>.bar"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe "main.context.data.<FieldAccess>.<unknown>.bar.<ReturnType>.<unknown>"
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
      x.methodFullName shouldBe "main.context.data1.<FieldAccess>.<unknown>.data2.<FieldAccess>.<unknown>.data3.<FieldAccess>.<unknown>.data4.<FieldAccess>.<unknown>.bar"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe "main.context.data1.<FieldAccess>.<unknown>.data2.<FieldAccess>.<unknown>.data3.<FieldAccess>.<unknown>.data4.<FieldAccess>.<unknown>.bar.<ReturnType>.<unknown>"
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

  "return statement having pointer to structure" should {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        |	name string
        |}
        |type Service struct {
        |	value *Node
        |}
        |func NewService(node *Node) *Service {
        |        return &Service{
        |                value: &Node{node},
        |        }
        |}
        |""".stripMargin)

    "check call node properties" in {
      cpg.call("Service").size shouldBe 1
      val List(callNode) = cpg.call("Service").l
      callNode.argument.size shouldBe 1
    }
  }

  "return statement having structure initialising with string" should {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        |	name string
        |}
        |type Service struct {
        |	value *Node
        |}
        |func NewService() *Service {
        |        return &Service{
        |                value: &Node{"somestring"},
        |        }
        |}
        |""".stripMargin)

    "check call node properties" in {
      cpg.call("Service").size shouldBe 1
      val List(callNode) = cpg.call("Service").l
      callNode.argument.size shouldBe 1
    }
  }
}
