package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class StructureCallTests extends GoCodeToCpgSuite(withOssDataflow = true) {

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
      rectangleCallNode.methodFullName shouldBe "main.Rectangle"
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

    "check no call node present" in {
      cpg.call.nameExact("Rectangle").size shouldBe 0
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
      rectangleCallNode.methodFullName shouldBe "path/to/somepackage.Rectangle"
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
    }

    "type full name of argument" ignore {
      squareArgument.typeFullName shouldBe "main.Square"
    }
  }

}
