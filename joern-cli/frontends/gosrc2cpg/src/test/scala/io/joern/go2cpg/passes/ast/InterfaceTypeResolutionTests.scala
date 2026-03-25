package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class InterfaceTypeResolutionTests extends GoCodeToCpgSuite {

  "Type assertion expression" should {
    val cpg = code("""
        |package main
        |type MyStruct struct {
        |  name string
        |}
        |func foo(x interface{}) {
        |  v := x.(MyStruct)
        |}
        |""".stripMargin)

    "produce a cast call with the correct type" in {
      val List(castCall) = cpg.call.name(Operators.cast).l
      castCall.typeFullName shouldBe "main.MyStruct"
      castCall.methodFullName shouldBe Operators.cast
      castCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "have the operand as an argument to the cast call" in {
      val List(castCall) = cpg.call.name(Operators.cast).l
      castCall.argument.size shouldBe 1
      val List(arg: Identifier) = castCall.argument.l: @unchecked
      arg.name shouldBe "x"
    }
  }

  "Interface with methods defined in same package" should {
    val cpg = code("""
        |package main
        |type Reader interface {
        |  Read(p []byte) (int, error)
        |  Close() error
        |}
        |func main() {}
        |""".stripMargin)

    "have a TypeDecl node for the interface" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Reader").l
      typeDeclNode.fullName shouldBe "main.Reader"
    }
  }

  "Method call on interface-typed receiver with method defined on struct" should {
    val cpg = code("""
        |package main
        |type Speaker interface {
        |  Speak() string
        |}
        |type Dog struct {
        |  name string
        |}
        |func (d Dog) Speak() string {
        |  return d.name
        |}
        |func greet(s Speaker) {
        |  var msg string = s.Speak()
        |}
        |""".stripMargin)

    "resolve the method call on the interface receiver" in {
      val speakCalls = cpg.call.name("Speak").l
      speakCalls.size shouldBe 1
      val List(speakCall) = speakCalls
      speakCall.typeFullName shouldBe "string"
    }
  }
}
