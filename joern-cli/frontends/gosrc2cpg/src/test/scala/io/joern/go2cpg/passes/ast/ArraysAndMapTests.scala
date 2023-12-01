package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import java.io.File
class ArraysAndMapTests extends GoCodeToCpgSuite {

  "Be correct when a int array is declared" should {
    val cpg = code("""
        |package main
        |func main() {
        |	var a []int
        |}
        |""".stripMargin)
    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]int"
    }
  }

  "Be correct when a int array is declared with array length" should {
    val cpg = code("""
        |package main
        |func main() {
        |	var a [2]int
        |}
        |""".stripMargin)
    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]int"
    }
  }

  "Be correct when a int array is declared with array length as global variable" should {
    val cpg = code("""
        |package main
        |var a [2]int
        |func main() {
        |}
        |""".stripMargin)
    "check Global member node" in {
      val List(x) = cpg.typeDecl("main").l
      val List(a) = x.member.l
      a.name shouldBe "a"
      a.typeFullName shouldBe "[]int"
    }
  }

  "Be correct when a int array is declared and initialised with array length as global variable" should {
    val cpg = code("""
        |package main
        |var a = [5]int{1,2}
        |func main() {
        |}
        |""".stripMargin)

    "check Global Member node" in {
      val List(x) = cpg.typeDecl("main").l
      val List(a) = x.member.l
      a.name shouldBe "a"
      a.typeFullName shouldBe "[]int"
    }

    // TODO need to be handled as part of initializer constructor implementation for package TypeDecl
    "Check Array initializer CALL node" ignore {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]int"
      val List(arg1: Literal, arg2: Literal) = x.argument.l: @unchecked
      arg1.code shouldBe "1"
      arg2.code shouldBe "2"
    }

    "Check assignment call node" ignore {
      val List(assignmentCallNode) = cpg.call(Operators.assignment).l
      assignmentCallNode.typeFullName shouldBe "[]int"
      val List(arg1: Identifier, arg2: Call) = assignmentCallNode.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg1.typeFullName shouldBe "[]int"
      arg2.typeFullName shouldBe "[]int"
    }

    "be correct with code field" ignore {
      val cpg = code("""
          |package main
          |var a = [5]int{1,2}
          |func main() {
          |}
          |""".stripMargin)
      val List(assignmentCallNode) = cpg.call(Operators.assignment).l
      // TODO: Fix the code format - there should be a = in between
      assignmentCallNode.code shouldBe "var a = [5]int{1,2}"
    }
  }

  "Be correct when a int array is initialized" should {
    val cpg = code("""
        |package main
        |func main() {
        |	a := [5]int{1,2}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]int"
      val List(arg1: Literal, arg2: Literal) = x.argument.l: @unchecked
      arg1.code shouldBe "1"
      arg2.code shouldBe "2"
    }

    "Check assignment call node" in {
      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l
      assignmentCallNode.typeFullName shouldBe "[]int"
      val List(arg1: Identifier, arg2: Call) = assignmentCallNode.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg1.typeFullName shouldBe "[]int"
      arg2.typeFullName shouldBe "[]int"
    }
  }

  "Be correct when a string array is initialized" should {
    val cpg = code("""
        |package main
        |func main() {
        |	a := [5]string{"hello","world"}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]string"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]string"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]string"
      val List(arg1: Literal, arg2: Literal) = x.argument.l: @unchecked
      arg1.code shouldBe "\"hello\""
      arg2.code shouldBe "\"world\""
    }

    "Check assignment call node" in {
      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l
      assignmentCallNode.typeFullName shouldBe "[]string"
      val List(arg1: Identifier, arg2: Call) = assignmentCallNode.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg1.typeFullName shouldBe "[]string"
      arg2.typeFullName shouldBe "[]string"
    }
  }

  "Be correct when a dynamic length int array is initialized" should {
    val cpg = code("""
        |package main
        |func main() {
        |	a := [...]int{1,2}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]int"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]int"
      val List(arg1: Literal, arg2: Literal) = x.argument.l: @unchecked
      arg1.code shouldBe "1"
      arg2.code shouldBe "2"
    }

    "Check assignment call node" in {
      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l
      assignmentCallNode.typeFullName shouldBe "[]int"
      assignmentCallNode.code shouldBe "a := [...]int{1,2}"

      val List(arg1: Identifier, arg2: Call) = assignmentCallNode.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg1.typeFullName shouldBe "[]int"
      arg2.typeFullName shouldBe "[]int"
    }
  }

  "Be correct when an empty array is initialized" should {
    val cpg = code("""
        |package main
        |func main() {
        |	a := [5]string{}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local("a").size shouldBe 1
      val List(x) = cpg.local("a").l
      x.typeFullName shouldBe "[]string"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.typeFullName shouldBe "[]string"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]string"
      x.argument.size shouldBe 0
    }

    "Check assignment call node" in {
      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l
      assignmentCallNode.typeFullName shouldBe "[]string"
      val List(arg1: Identifier, arg2: Call) = assignmentCallNode.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg1.typeFullName shouldBe "[]string"
      arg2.typeFullName shouldBe "[]string"
    }
  }

  "be correct when array of struct within package same file" should {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        |name string
        |}
        |
        |func main() {
        | var a Node = Node{"value1"}
        | var b Node = Node{"value2"}
        |
        | c := []Node{a, b}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local.size shouldBe 3
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]main.Node"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier.size shouldBe 5
      val List(a, b, c, d, e) = cpg.identifier.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]main.Node"
      d.typeFullName shouldBe "main.Node"
      e.typeFullName shouldBe "main.Node"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]main.Node"
      val List(arg1: Identifier, arg2: Identifier) = x.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg2.name shouldBe "b"
    }

    "Check assignment call node" in {
      val List(first, sec, third) = cpg.call(Operators.assignment).l
      first.typeFullName shouldBe "main.Node"
      sec.typeFullName shouldBe "main.Node"
      third.typeFullName shouldBe "[]main.Node"
      val List(arg1: Identifier, arg2: Call) = third.argument.l: @unchecked
      arg1.name shouldBe "c"
      arg1.typeFullName shouldBe "[]main.Node"
      arg2.typeFullName shouldBe "[]main.Node"
    }
  }

  "be correct when array of struct within package but different file" should {
    val cpg = code(
      """
        |package main
        |
        |type Node struct {
        |name string
        |}
        |
        |""".stripMargin,
      "first.go"
    )
      .moreCode(
        """
          |package main
          |
          |func main() {
          | var a Node = Node{"value1"}
          | var b Node = Node{"value2"}
          |
          | c := []Node{a, b}
          |}
          |""".stripMargin,
        "second.go"
      )

    "check FILE Nodes" in {
      cpg.file.size shouldBe 4
    }

    "check LOCAL node" in {
      cpg.local.size shouldBe 3
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]main.Node"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier.size shouldBe 5
      val List(a, b, c, d, e) = cpg.identifier.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]main.Node"
      d.typeFullName shouldBe "main.Node"
      e.typeFullName shouldBe "main.Node"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]main.Node"
      val List(arg1: Identifier, arg2: Identifier) = x.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg2.name shouldBe "b"
    }

    "Check assignment call node" in {
      val List(first, sec, third) = cpg.call(Operators.assignment).l
      first.typeFullName shouldBe "main.Node"
      sec.typeFullName shouldBe "main.Node"
      third.typeFullName shouldBe "[]main.Node"
      val List(arg1: Identifier, arg2: Call) = third.argument.l: @unchecked
      arg1.name shouldBe "c"
      arg1.typeFullName shouldBe "[]main.Node"
      arg2.typeFullName shouldBe "[]main.Node"
    }
  }

  "be correct when array of struct in different package but same project" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Node struct {
        |  name string
        |}
        |""".stripMargin,
      Seq("fpkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func main() {
        | var a fpkg.Node = fpkg.Node{"value1"}
        | var b fpkg.Node = fpkg.Node{"value2"}
        |
        | c := []fpkg.Node{a, b}
        |}
        |""".stripMargin,
      "main.go"
    )

    "check FILE Nodes" in {
      cpg.file.size shouldBe 5
    }

    "check LOCAL node" in {
      cpg.local.size shouldBe 3
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      b.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      c.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier.size shouldBe 5
      val List(a, b, c, d, e) = cpg.identifier.l
      a.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      b.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      c.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
      d.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      e.typeFullName shouldBe "joern.io/sample/fpkg.Node"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
      val List(arg1: Identifier, arg2: Identifier) = x.argument.l: @unchecked
      arg1.name shouldBe "a"
      arg2.name shouldBe "b"
    }

    "Check assignment call node" in {
      val List(first, sec, third) = cpg.call(Operators.assignment).l
      first.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      sec.typeFullName shouldBe "joern.io/sample/fpkg.Node"
      third.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
      val List(arg1: Identifier, arg2: Call) = third.argument.l: @unchecked
      arg1.name shouldBe "c"
      arg1.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
      arg2.typeFullName shouldBe "[]joern.io/sample/fpkg.Node"
    }
  }

  "be correct when when a int array id initialized having pointer" should {
    val cpg = code("""
        |package main
        |func main() {
        | var a int = 1
        | var b int = 2
        | c := [5]*int{&a,&b}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local.size shouldBe 3
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "int"
      c.typeFullName shouldBe "[]*int"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier.size shouldBe 5
      val List(a, b, c, d, e) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "int"
      c.typeFullName shouldBe "[]*int"
      d.typeFullName shouldBe "int"
      e.typeFullName shouldBe "int"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]*int"
      val List(arg1: Call, arg2: Call) = x.argument.l: @unchecked
      arg1.name shouldBe Operators.addressOf
      arg1.typeFullName shouldBe "*int"
      arg2.name shouldBe Operators.addressOf
      arg2.typeFullName shouldBe "*int"
    }

    "Check assignment call node" in {
      val List(first, sec, third) = cpg.call(Operators.assignment).l
      first.typeFullName shouldBe "int"
      sec.typeFullName shouldBe "int"
      third.typeFullName shouldBe "[]*int"
      val List(arg1: Identifier, arg2: Call) = third.argument.l: @unchecked
      arg1.name shouldBe "c"
      arg1.typeFullName shouldBe "[]*int"
      arg2.typeFullName shouldBe "[]*int"
    }
  }

  "be correct when array of struct having pointer within package same file" should {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        | name string
        |}
        |
        |func main() {
        | var a Node = Node{"value1"}
        | var b Node = Node{"value2"}
        |
        | c := []*Node{&a,&b}
        |}
        |""".stripMargin)

    "check LOCAL node" in {
      cpg.local.size shouldBe 3
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]*main.Node"
    }

    "Check IDENTIFIER node" in {
      cpg.identifier.size shouldBe 5
      val List(a, b, c, d, e) = cpg.identifier.l
      a.typeFullName shouldBe "main.Node"
      b.typeFullName shouldBe "main.Node"
      c.typeFullName shouldBe "[]*main.Node"
      d.typeFullName shouldBe "main.Node"
      e.typeFullName shouldBe "main.Node"
    }

    "Check Array initializer CALL node" in {
      val List(x) = cpg.call(Operators.arrayInitializer).l
      x.typeFullName shouldBe "[]*main.Node"
      val List(arg1: Call, arg2: Call) = x.argument.l: @unchecked
      arg1.name shouldBe Operators.addressOf
      arg1.typeFullName shouldBe "*main.Node"
      arg2.name shouldBe Operators.addressOf
      arg2.typeFullName shouldBe "*main.Node"
    }

    "Check assignment call node" in {
      val List(first, sec, third) = cpg.call(Operators.assignment).l
      first.typeFullName shouldBe "main.Node"
      sec.typeFullName shouldBe "main.Node"
      third.typeFullName shouldBe "[]*main.Node"
      val List(arg1: Identifier, arg2: Call) = third.argument.l: @unchecked
      arg1.name shouldBe "c"
      arg1.typeFullName shouldBe "[]*main.Node"
      arg2.typeFullName shouldBe "[]*main.Node"
    }
  }

  "be correct when array access using variable having single index" should {
    val cpg = code("""
        |package main
        |func main() {
        | var myArray []int = []int{1, 2}
        | value := myArray[0]
        |}
        |""".stripMargin)

    "be correct for local and identifier node" in {
      val List(myArr, _) = cpg.local.l
      myArr.typeFullName shouldBe "[]int"

      val List(identifier, _, _) = cpg.identifier.l
      identifier.typeFullName shouldBe "[]int"
    }
    "be correct for indexAccess call node" in {
      val List(indexCall) = cpg.call.name("<operator>.indexAccess").l
      indexCall.code shouldBe "myArray[0]"
      indexCall.methodFullName shouldBe "<operator>.indexAccess"
      indexCall.typeFullName shouldBe "int"

      val List(indexIdentifier: Identifier, indexLiteral: Literal) = indexCall.argument.l: @unchecked

      indexIdentifier.code shouldBe "myArray"
      indexIdentifier.typeFullName shouldBe "[]int"

      indexLiteral.code shouldBe "0"
    }
  }

  "be correct when array of pointer access using variable having single index" should {
    val cpg = code("""
        |package main
        |func main() {
        | var myArray []*int
        | value := myArray[0]
        |}
        |""".stripMargin)

    "be correct for local and identifier node" in {
      val List(myArr, _) = cpg.local.l
      myArr.typeFullName shouldBe "[]*int"

      val List(identifier, _, _) = cpg.identifier.l
      identifier.typeFullName shouldBe "[]*int"
    }
    "be correct for indexAccess call node" in {
      val List(indexCall) = cpg.call.name("<operator>.indexAccess").l
      indexCall.code shouldBe "myArray[0]"
      indexCall.methodFullName shouldBe "<operator>.indexAccess"
      indexCall.typeFullName shouldBe "*int"

      val List(indexIdentifier: Identifier, indexLiteral: Literal) = indexCall.argument.l: @unchecked

      indexIdentifier.code shouldBe "myArray"
      indexIdentifier.typeFullName shouldBe "[]*int"

      indexLiteral.code shouldBe "0"
    }
  }

  "be correct when pointer of array access using variable having single index" should {
    val cpg = code("""
        |package main
        |func main() {
        | var myArray *[]int
        | value := (*myArray)[0]
        |}
        |""".stripMargin)

    "be correct for local and identifier node" in {
      val List(myArr, _) = cpg.local.l
      myArr.typeFullName shouldBe "*[]int"

      val List(identifier, _, _) = cpg.identifier.l
      identifier.typeFullName shouldBe "*[]int"
    }
    "be correct for indexAccess call node" in {
      val List(indexCall) = cpg.call.name("<operator>.indexAccess").l
      indexCall.code shouldBe "(*myArray)[0]"
      indexCall.methodFullName shouldBe "<operator>.indexAccess"
      indexCall.typeFullName shouldBe "int"

      val List(indexIdentifier: Call, indexLiteral: Literal) = indexCall.argument.l: @unchecked

      indexIdentifier.code shouldBe "*myArray"
      indexIdentifier.typeFullName shouldBe "*[]int"

      indexLiteral.code shouldBe "0"
    }
  }

  "be correct when array access using variable having multi index" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var myArray [][]string = [][]string{{"1", "2"}, {"3", "4"}}
        |   value := myArray[0][1]
        |}
        |""".stripMargin)
    "Be correct for local and Identifier nodes" in {
      val List(myArr, _) = cpg.local.l
      myArr.typeFullName shouldBe "[][]string"

      val List(identifier, _, _) = cpg.identifier.l
      identifier.typeFullName shouldBe "[][]string"
    }
    "Be correct for IndexAccess call nodes" in {
      val List(indexCallFirst, _) = cpg.call.name(Operators.indexAccess).l
      indexCallFirst.code shouldBe "myArray[0][1]"
      indexCallFirst.typeFullName shouldBe "string"

      val List(indexIdentifier: Call, indexLiteral1: Literal) = indexCallFirst.argument.l: @unchecked
      indexIdentifier.code shouldBe "myArray[0]"
      indexIdentifier.typeFullName shouldBe "[]string"
      indexLiteral1.code shouldBe "1"

      val List(indexIdentifierTwo: Identifier, indexLiteral2: Literal) = indexIdentifier.argument.l: @unchecked
      indexIdentifierTwo.code shouldBe "myArray"
      indexIdentifierTwo.typeFullName shouldBe "[][]string"
      indexLiteral2.code shouldBe "0"
    }
  }

  "be correct when map access using string having single index" ignore {
    val cpg = code("""
        |package main
        |func main() {
        | var mymap map[string]int = make(map[string]int)
        | var a = mymap["key"]
        |}
        |""".stripMargin)
    "be correct for local node properties" in {
      val List(x) = cpg.local("mymap").l
      x.typeFullName shouldBe "map[]int"
    }

    "be correct for index access" in {
      val List(indexCall) = cpg.call.name("<operator>.indexAccess").l
      indexCall.code shouldBe "mymap[\"key\"]"
      indexCall.methodFullName shouldBe "<operator>.indexAccess"
      indexCall.typeFullName shouldBe "int"

      val List(indexLiteral: Literal, indexIdentifier: Identifier) = indexCall.argument.l: @unchecked

      indexIdentifier.code shouldBe "mymap"
      indexIdentifier.typeFullName shouldBe "map[]int"

      indexLiteral.code shouldBe "\"key\""
      indexLiteral.typeFullName shouldBe "string"
    }
  }

  "be correct when struct array access using variable having single index" should {
    val cpg = code("""
         |package main
         |type Person struct {
         |    Fname string
         |    Lname string
         |}
         |func (p Person) fullName() string {
         |	return p.Fname + " " + p.Lname
         |}
         |func main() {
         |  var person [3]Person
         |  var a = person[0]
         |  var flname = person[0].fullName()
         |}
         |""".stripMargin)

    "local and identifer checks" in {
      val List(person, _, _) = cpg.local.l
      person.typeFullName shouldBe "[]main.Person"
    }

    "index access call check" in {
      val List(indexOne, indexTwo) = cpg.call.name(Operators.indexAccess).l
      indexOne.code shouldBe "person[0]"
      indexOne.methodFullName shouldBe Operators.indexAccess
      indexOne.typeFullName shouldBe "main.Person"

      val List(indexIdentifier: Identifier, indexLiteral1: Literal) = indexOne.argument.l: @unchecked
      indexIdentifier.code shouldBe "person"
      indexIdentifier.typeFullName shouldBe "[]main.Person"
      indexLiteral1.code shouldBe "0"
    }

    "be correct for method call node on array index access" in {
      val List(fullName) = cpg.call("fullName").l
      fullName.methodFullName shouldBe "main.Person.fullName"
      fullName.typeFullName shouldBe "string"

      val List(args: Call) = fullName.argument.l: @unchecked
      args.argumentIndex shouldBe 0
      args.name shouldBe Operators.indexAccess
      args.typeFullName shouldBe "main.Person"
    }
  }
}
