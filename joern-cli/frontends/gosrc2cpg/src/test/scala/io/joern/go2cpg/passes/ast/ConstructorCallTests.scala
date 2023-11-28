package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.List

class ConstructorCallTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "when structure declared with multiple argument" should {
    val cpg = code(
      """
        package main
        |func main() {
        |    rect := Rectangle{width: 10, height: "somestr"}
        |}
        |""".stripMargin,
      "test.go"
    )
    val List(rectangleCallNode) = cpg.call.nameExact("Rectangle").l
    "test basic cpg properties for call node of structure declaration" in {
      rectangleCallNode.name shouldBe "Rectangle"
      rectangleCallNode.methodFullName shouldBe "main.Rectangle.<init>"
      rectangleCallNode.typeFullName shouldBe "main.Rectangle"
    }

    "test argument of structure declaration node(call node)" in {
      val List(widthNode, heightNode) = rectangleCallNode.argument.isLiteral.l
      widthNode.typeFullName shouldBe "int"
      heightNode.typeFullName shouldBe "string"
    }

    "test argument index" in {
      val List(widthNode, heightNode) = rectangleCallNode.argument.isLiteral.l
      widthNode.argumentIndex shouldBe 1
      heightNode.argumentIndex shouldBe 2
    }

  }

  "when structure declared with no argument" should {
    val cpg = code(
      """
        package main
        |func main() {
        |    rect := Rectangle{}
        |}
        |""".stripMargin,
      "test.go"
    )

    "check call node present" in {
      cpg.call.nameExact("Rectangle").size shouldBe 1
    }
  }

  "when is structure imported from other package structure" should {
    val cpg = code(
      """
        package main
        |import (
        |    "path/to/somepackage"
        |)
        |func main() {
        |    rect := somepackage.Rectangle{l: 10, w: 20}
        |}
        |""".stripMargin,
      "test.go"
    )
    val List(rectangleCallNode) = cpg.call.nameExact("Rectangle").l
    "test basic cpg properties for call node of structure declaration" in {
      rectangleCallNode.name shouldBe "Rectangle"
      rectangleCallNode.methodFullName shouldBe "path/to/somepackage.Rectangle.<init>"
      rectangleCallNode.typeFullName shouldBe "path/to/somepackage.Rectangle"
      rectangleCallNode.signature shouldBe "path/to/somepackage.Rectangle()"
    }

    "test argument of structure declaration node(call node)" in {
      val List(lNode, wNode) = cpg.call.nameExact("Rectangle").argument.isLiteral.l
      lNode.typeFullName shouldBe "int"
      wNode.typeFullName shouldBe "int"
    }
  }

  "when one structure is passed as parameter to another structure's declaration" should {
    val cpg = code(
      """
        package main
        |
        |func main() {
        |	square := Square{length: 10, width: 20}
        |	rect := Rectangle{sq: square}
        |}
        |
        |""".stripMargin,
      "test.go"
    )
    val List(squareArgument) = cpg.call.nameExact("Rectangle").argument.isIdentifier.l
    "check argument of Rectangle call node" in {
      squareArgument.name shouldBe "square"
      squareArgument.argumentIndex shouldBe 1
      squareArgument.order shouldBe 1
      squareArgument.code shouldBe "square"
      squareArgument.typeFullName shouldBe "main.Square"
    }
  }

  "when structure is defined via var keyword with argument keyword(length)" should {
    val cpg = code(
      """
        package main
        |
        |type Square struct {
        |	length float64
        |}
        |
        |func main() {
        |	var square = Square{length: 10}
        |	fmt.Println(square)
        |}
        |""".stripMargin,
      "test.go"
    )

    "check argument of Square call node" in {
      val List(squareCallNode) = cpg.call("Square").l
      val List(squareArgument) = squareCallNode.argument.isLiteral.l
      squareArgument.argumentIndex shouldBe 1
      squareArgument.order shouldBe 1
      squareArgument.code shouldBe "10"
      squareArgument.typeFullName shouldBe "int"
    }

    "Single call node getting created for Structure declaration" in {
      cpg.call("Square").size shouldBe 1
    }
  }

  "when structure is defined via var keyword without argument name keyword" should {
    val cpg = code(
      """
        package main
        |
        |type Square struct {
        |	length float64
        |}
        |
        |func main() {
        |	var square = Square{10}
        |	fmt.Println(square)
        |
        |}
        |""".stripMargin,
      "test.go"
    )

    "check argument of Square call node" in {
      val List(squareCallNode) = cpg.call("Square").l
      val List(squareArgument) = squareCallNode.argument.isLiteral.l
      squareArgument.argumentIndex shouldBe 1
      squareArgument.order shouldBe 1
      squareArgument.code shouldBe "10"
      squareArgument.typeFullName shouldBe "int"
    }

    "Single call node getting created for Structure declaration" in {
      cpg.call("Square").size shouldBe 1
    }
  }

  "when structure is defined via var keyword without argument name keyword and multiple argument" should {
    val cpg = code(
      """
        package main
        |
        |type Rectangle struct {
        |	length float64
        |   width float64
        |}
        |
        |func main() {
        |	var rectangle = Rectangle{10, 20}
        |	fmt.Println(rectangle)
        |}
        |""".stripMargin,
      "test.go"
    )

    "check argument of Rectangle call node" in {
      val List(squareCallNode)                               = cpg.call("Rectangle").l
      val List(rectangleArgumentFirst, squareArgumentSecond) = squareCallNode.argument.isLiteral.l
      rectangleArgumentFirst.argumentIndex shouldBe 1
      rectangleArgumentFirst.order shouldBe 1
      rectangleArgumentFirst.code shouldBe "10"
      rectangleArgumentFirst.typeFullName shouldBe "int"

      squareArgumentSecond.argumentIndex shouldBe 2
      squareArgumentSecond.order shouldBe 2
      squareArgumentSecond.code shouldBe "20"
      squareArgumentSecond.typeFullName shouldBe "int"
    }

    "Single call node getting created for Structure declaration" in {
      cpg.call("Rectangle").size shouldBe 1
    }
  }

}
