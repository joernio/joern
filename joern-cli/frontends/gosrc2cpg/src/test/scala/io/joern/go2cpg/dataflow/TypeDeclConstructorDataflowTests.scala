package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class TypeDeclConstructorDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "structure initialization having array of complex(structure-array) type" should {

    val cpg = code("""
        |package main
        |
        |import "fmt"
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
        |	var fphone = []Phone{{phone: "1234567890", phonetype: "Home"}, {phone: "1234567890", phonetype: "Home"}}
        |	var person = Person{fphone, "Peter"}
        |   fmt.Println(person)
        |}
        |""".stripMargin)

    "Check array initilization to final node creation dataflow" in {
      val source = cpg.identifier("fphone")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 2
    }

    "Check constructor parameter to println dataflow" ignore {
      val sink = cpg.call("Println")

      var source = cpg.literal("\"1234567890\"")
      sink.reachableByFlows(source).size shouldBe 1

      source = cpg.literal("\"Home\"")
      sink.reachableByFlows(source).size shouldBe 2
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

    "Check array literal to Println dataflow" in {
      val source = cpg.literal("1")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 1
    }

    "Check constructor parameter to Println dataflow - 1" in {
      val source = cpg.literal("\"Peter\"")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 1
    }

    "Check constructor parameter to Println dataflow - 2" in {
      val source = cpg.identifier("numbers")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 2
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
        |    tmp := person
        |    fmt.Println(tmp)
        |}
        |""".stripMargin)

    "Check dataflow from every constructor parameter node to assignment" in {
      val sink = cpg.identifier("person").l

      val source1 = cpg.literal.code("\"John Doe\"").l
      sink.reachableByFlows(source1).size shouldBe 2

      val source2 = cpg.literal.code("\"New York\"").l
      sink.reachableByFlows(source2).size shouldBe 2

      val source3 = cpg.literal.code("\"123 Main St\"").l
      sink.reachableByFlows(source3).size shouldBe 2

      val source4 = cpg.literal.code("30").l
      sink.reachableByFlows(source4).size shouldBe 2

      val source5 = cpg.literal.code("\"USA\"").l
      sink.reachableByFlows(source5).size shouldBe 2

    }

    "Check dataflow from variable holding structure to println" in {
      val source = cpg.identifier("person")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 2
    }

    "Check dataflow from constructor parameter to identifier" in {
      val source = cpg.literal.code("\"New York\"")
      val sink   = cpg.identifier("person")
      sink.reachableByFlows(source).size shouldBe 2
    }

    "Check dataflow from constructor parameter to another assignment" in {
      val source = cpg.literal.code("\"New York\"")
      val sink   = cpg.identifier("tmp")
      sink.reachableByFlows(source).size shouldBe 2
    }

  }

}
