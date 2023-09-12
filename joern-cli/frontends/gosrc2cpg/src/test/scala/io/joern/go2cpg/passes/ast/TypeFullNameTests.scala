package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.semanticcpg.language.*

import java.io.File

class TypeFullNameTests extends GoCodeToCpgSuite {
  "Type check for declared primitive types" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var a int = 1
        |   var b, c float32
        |   var d []int
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c, d) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }
  }

  "Type check for implicit Type based on assigned literal" ignore {
    val cpg = code("""
        |package main
        |func main() {
        |   var a = 10
        |   var b = 20.5
        |   var c = [5]int{1,2}
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "[]int"
    }
  }

  "Method call return value assigned to variable type check" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |type Person struct{
        |   fname string
        |   lname string
        |}
        |func (person Person) fullName() string {
        |	return person.fname + " " + person.lname
        |}
        |""".stripMargin,
      Seq("lib", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package fpkg
        |import "joern.io/sample/lib"
        |func bar() string{
        |  return "somestr"
        |}
        |func createPerson(fn, ln string) lib.Person {
        |  return lib.Person{fname: fn, lname: ln}
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |import "joern.io/sample/lib"
        |func foo() {
        |  var a = fpkg.bar()
        |  var per = fpkg.createPerson("Pandurang", "Patil")
        |  var compName = per.fullName()
        |  var perOne lib.Person = fpkg.createPerson("Sameer", "Shinde")
        |  var compNameOne = perOne.fullName()
        |  var perThree = lib.Person{fname: "Ram", lname: "Thakur"}
        |}
        |""".stripMargin,
      "main.go"
    )

    "Call node typeFullName check with primitive return type" in {
      val List(bar) = cpg.call("bar").l
      bar.typeFullName shouldBe "string"
    }

    "Call node typeFullName check with struct return type" in {
      val List(createPerson) = cpg.call("createPerson").lineNumber(7).l
      createPerson.typeFullName shouldBe "joern.io/sample/lib.Person"
    }

    "Call node typeFullName check for function call on receiver object usecase 1" ignore {
      val List(fullName) = cpg.call("fullName").lineNumber(8).l
      fullName.typeFullName shouldBe "string"
    }

    "Call node typeFullName check for function call on receiver object usecase 2" in {
      val List(fullName) = cpg.call("fullName").lineNumber(10).l
      fullName.typeFullName shouldBe "string"
    }

    "TODO variable type checks not working " ignore {
      val List(a) = cpg.local("a").l
      a.typeFullName shouldBe "string"

      val List(aident) = cpg.identifier("a").lineNumber(6).l
      aident.typeFullName shouldBe "string"

      val List(pident) = cpg.identifier("per").lineNumber(7).l
      pident.typeFullName shouldBe "joern.io/sample/lib.Person"

      val List(compName) = cpg.identifier("compName").lineNumber(8).l
      compName.typeFullName shouldBe "string"

      val List(compNameOne) = cpg.identifier("compNameOne").lineNumber(10).l
      compNameOne.typeFullName shouldBe "string"

      val List(perThree) = cpg.identifier("perThree").lineNumber(11).l
      perThree.typeFullName shouldBe "joern.io/sample/lib.Person"

    }

    "variable type checks working" in {
      val List(perOne) = cpg.identifier("perOne").lineNumber(9).l
      perOne.typeFullName shouldBe "joern.io/sample/lib.Person"
    }
  }
}
