package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

import java.io.File

class MethodTests extends GoCodeToCpgSuite {

  "Empty parameters with no return" should {
    val cpg = code("""
        |package main
        |func foo() {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() {")
      x.signature shouldBe "main.foo()"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "check empty return node" in {
      cpg.method.name("foo").methodReturn.size shouldBe 1
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.typeFullName shouldBe Defines.voidTypeName
    }
  }

  "Empty parameters with int return" should {
    val cpg = code("""
        |package main
        |func foo() int{
        |  return 0
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo()")
      x.signature shouldBe "main.foo()int"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(5)
    }

    "Be correct with return node" in {
      cpg.method.name("foo").methodReturn.size shouldBe 1
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.typeFullName shouldBe "int"
    }
  }

  "Empty parameters with int array return" should {
    val cpg = code("""
        |package main
        |func foo() [5]int {
        |	a := [5]int{1, 2}
        |	return a
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() [5]int {")
      x.signature shouldBe "main.foo()[]int"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(6)
    }

    "Be correct with return node" in {
      cpg.method.name("foo").methodReturn.size shouldBe 1
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.typeFullName shouldBe "[]int"
    }
  }

  "Empty parameters with struct type return" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() fpkg.Sample{
        |  return nil
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() fpkg.Sample{")
      x.signature shouldBe "main.foo()joern.io/sample/fpkg.Sample"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(6)
    }

    "Be correct with return node" in {
      cpg.method.name("foo").methodReturn.size shouldBe 1
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.typeFullName shouldBe "joern.io/sample/fpkg.Sample"
    }
  }

  "Empty parameters with tuple return" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() (fpkg.Sample,error){
        |  return nil
        |}
        |""".stripMargin)

    "Be correct with method node properties" ignore {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() (fpkg.Sample,error){")
      // TODO: Tuple handling needs to be done properly to return both the types.
      x.signature shouldBe "main.foo()(joern.io/sample/fpkg.Sample,error)"
      x.isExternal shouldBe false

      x.order shouldBe 2
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(6)
    }

    "Be correct with return node" in {
      cpg.method.name("foo").methodReturn.size shouldBe 1
      val List(x) = cpg.method.name("foo").methodReturn.l
      // TODO: Tuple handling needs to be done properly to return both the types.
      x.typeFullName shouldBe "joern.io/sample/fpkg.Sample"
    }
  }

  "Method called from another method first in the code" should {
    val cpg = code("""
        |package main
        |func foo() {
        |  bar()
        |}
        |func bar() {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() {")
      x.signature shouldBe "main.foo()"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(5)

      val List(y) = cpg.method.name("bar").l
      y.name shouldBe "bar"
      y.fullName shouldBe "main.bar"
      y.code should startWith("func bar() {")
      y.signature shouldBe "main.bar()"
      y.isExternal shouldBe false

      y.order shouldBe 1
      y.filename shouldBe "Test0.go"
      y.lineNumber shouldBe Option(6)
      y.lineNumberEnd shouldBe Option(7)
    }

  }

  "Method arguments with primitive types" should {
    val cpg = code("""
                     |package main
                     |func foo(argc int, argv string) {
                     |}
                     |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv string)")
      x.signature shouldBe "main.foo(int, string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "More than one arguments of same Type" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc, arga int, argv string)")
      x.signature shouldBe "main.foo(int, int, string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "arga", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga int"
      arga.order shouldBe 2
      arga.typeFullName shouldBe "int"
      arga.isVariadic shouldBe false
      arga.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv string"
      argv.order shouldBe 3
      argv.typeFullName shouldBe "string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, arga, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      arga.name shouldBe "arga"
      argv.name shouldBe "argv"
    }
  }

  "Variable argument use case" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv ...string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc, arga int, argv ...string)")
      x.signature shouldBe "main.foo(int, int, []string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }
    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "arga", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga int"
      arga.order shouldBe 2
      arga.typeFullName shouldBe "int"
      arga.isVariadic shouldBe false
      arga.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv ...string"
      argv.order shouldBe 3
      argv.typeFullName shouldBe "[]string"
      argv.isVariadic shouldBe true
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, arga, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      arga.name shouldBe "arga"
      argv.name shouldBe "argv"
    }
  }

  "Primitive Array argument use case" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv []int) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc, arga int, argv []int)")
      x.signature shouldBe "main.foo(int, int, []int)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }
    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "arga", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga int"
      arga.order shouldBe 2
      arga.typeFullName shouldBe "int"
      arga.isVariadic shouldBe false
      arga.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv []int"
      argv.order shouldBe 3
      argv.typeFullName shouldBe "[]int"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, arga, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      arga.name shouldBe "arga"
      argv.name shouldBe "argv"
    }
  }

  "Method arguments with primitive pointer" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv *string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv *string)")
      x.signature shouldBe "main.foo(int, *string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv *string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "*string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Method arguments with primitive pointer to pointer" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv **string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" ignore {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv **string)")
      // TODO: pointer to pointer use cae need to be hanled
      x.signature shouldBe "main.foo(int, **string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" ignore {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv **string"
      argv.order shouldBe 2
      // TODO: pointer to pointer use cae need to be hanled
      argv.typeFullName shouldBe "**string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Method arguments with array of primitive pointer" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv []*string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv []*string)")
      x.signature shouldBe "main.foo(int, []*string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv []*string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]*string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Variable argument of primitive pointer" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv ...*string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv ...*string)")
      x.signature shouldBe "main.foo(int, []*string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv ...*string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]*string"
      argv.isVariadic shouldBe true
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Method arguments with pointer of primitive array" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv *[]string) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv *[]string)")
      x.signature shouldBe "main.foo(int, *[]string)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv *[]string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "*[]string"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from same 'main' package and same file" should {
    val cpg = code("""
        |package main
        |type Sample struct {
        |	name int
        |}
        |
        |func foo(argc int, argv Sample) {
        |}
        |""".stripMargin)
    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv Sample)")
      x.signature shouldBe "main.foo(int, main.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(7)
      x.lineNumberEnd shouldBe Option(8)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "main.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from same 'main' package but different file" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      "lib.go"
    ).moreCode(
      """
        |package main
        |func foo(argc int, argv Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv Sample)")
      x.signature shouldBe "main.foo(int, main.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "main.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from same 'sample' package but different file" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package sample
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      "lib.go"
    ).moreCode(
      """
        |package sample
        |func foo(argc int, argv Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "joern.io/sample.foo"
      x.code should startWith("func foo(argc int, argv Sample)")
      x.signature shouldBe "joern.io/sample.foo(int, joern.io/sample.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "joern.io/sample.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from other package from same project" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      Seq("fpkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo(argc int, argv fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv fpkg.Sample)")
      x.signature shouldBe "main.foo(int, joern.io/sample/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "joern.io/sample/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from other package as variable argument" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      Seq("fpkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo(argc int, argv ...fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv ...fpkg.Sample)")
      x.signature shouldBe "main.foo(int, []joern.io/sample/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv ...fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]joern.io/sample/fpkg.Sample"
      argv.isVariadic shouldBe true
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv fpkg.Sample)")
      x.signature shouldBe "main.foo(int, privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "struct type array argument from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv []fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv []fpkg.Sample)")
      x.signature shouldBe "main.foo(int, []privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv []fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "pointer of struct type argument from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv *fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv *fpkg.Sample)")
      x.signature shouldBe "main.foo(int, *privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv *fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "*privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Array of struct pointer argument from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv []*fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv []*fpkg.Sample)")
      x.signature shouldBe "main.foo(int, []*privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv []*fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]*privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Pointer to struct array argument from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv *[]fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv *[]fpkg.Sample)")
      x.signature shouldBe "main.foo(int, *[]privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv *[]fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "*[]privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Variable argument of struct pointer from third party dependency" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import "privado.ai/test/fpkg"
        |func foo(argc int, argv ...*fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv ...*fpkg.Sample)")
      x.signature shouldBe "main.foo(int, []*privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv ...*fpkg.Sample"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "[]*privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe true
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "Method arguments with interface{} and any types along with array, pointer and variable argument of interface{}" should {
    val cpg = code("""
        |package main
        |func foo(argc any, argv interface{}, arga []interface{}, argb *interface{}, argd ...interface{}) {
        |}
        |""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith(
        "func foo(argc any, argv interface{}, arga []interface{}, argb *interface{}, argd ...interface{})"
      )
      x.signature shouldBe "main.foo(any, any, []any, *any, []any)"
      x.isExternal shouldBe false

      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv", "arga", "argb", "argd")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc any"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "any"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv interface{}"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "any"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga []interface{}"
      arga.order shouldBe 3
      arga.typeFullName shouldBe "[]any"
      arga.isVariadic shouldBe false
      arga.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argb) = cpg.parameter.name("argb").l
      argb.code shouldBe "argb *interface{}"
      argb.order shouldBe 4
      argb.typeFullName shouldBe "*any"
      argb.isVariadic shouldBe false
      argb.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING

      val List(argd) = cpg.parameter.name("argd").l
      argd.code shouldBe "argd ...interface{}"
      argd.order shouldBe 5
      argd.typeFullName shouldBe "[]any"
      argd.isVariadic shouldBe true
      argd.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argb").method.name.l shouldBe List("foo")
      cpg.parameter.name("argd").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv, arga, argb, argd) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
      arga.name shouldBe "arga"
      argb.name shouldBe "argb"
      argd.name shouldBe "argd"
    }
  }

  "struct type argument from third party dependency imported as . " should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |import . "privado.ai/test/fpkg"
        |func foo(argc int, argv Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct with method node properties" ignore {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv Sample)")
      // TODO: wrong methodfull name being genearted when the packaged is imported with '.'
      x.signature shouldBe "main.foo(int, privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false

      x.order shouldBe 2
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }

    "be correct for parameter nodes" ignore {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false
      argc.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv Sample"
      argv.order shouldBe 2
      // TODO: wrong methodfull name being genearted when the packaged is imported with '.'
      argv.typeFullName shouldBe "privado.ai/test/fpkg.Sample"
      argv.isVariadic shouldBe false
      argv.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "When ast of struct node is coming under return statement  and defined later" should {
    val cpg = code("""
        |package main
        |
        |func foo() Node {
        |   var boo = int64(0)
        |   return Node{
        |     value: boo,
        |   }
        |}
        |
        |type Node struct {
        |   value string
        |}""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method("foo").l
      x.fullName shouldBe "main.foo"
      // TODO: confirm if signature is correct
      x.signature shouldBe "main.foo()main.Node"
    }

    "Be correct with typeDecl node properties" in {
      val List(x) = cpg.typeDecl("Node").l
      x.fullName shouldBe "main.Node"
      x.fullName shouldBe "main.Node"
      x.member.size shouldBe 1

      val List(m) = x.member.l
      m.name shouldBe "value"
      m.typeFullName shouldBe "string"
    }

  }

  "When ast of struct node is coming under method's body and defined later" should {
    val cpg = code("""
        |package main
        |
        |func foo() Node {
        |   var boo = int64(0)
        |   var a = Node{
        |     value: boo,
        |   }
        |   return a
        |}
        |
        |type Node struct {
        |   value string
        |}""".stripMargin)

    "Be correct with method node properties" in {
      val List(x) = cpg.method("foo").l
      x.fullName shouldBe "main.foo"
      // TODO: confirm if signature is correct
      x.signature shouldBe "main.foo()main.Node"
    }

    "Be correct with typeDecl node properties" in {
      val List(x) = cpg.typeDecl("Node").l
      x.fullName shouldBe "main.Node"
      x.fullName shouldBe "main.Node"
      x.member.size shouldBe 1

      val List(m) = x.member.l
      m.name shouldBe "value"
      m.typeFullName shouldBe "string"
    }

  }

  "when function is passed as argument" should {
    val cpg = code("""
        |package main
        |
        |func help() {
        |   Person(Hello, "hi")
        |}
        |
        |func Hello(value string) {}
        |
        |func Person(hello func(string), value string) {
        |   hello(value)
        |}
        |""".stripMargin)

    // TODO: Handle function as argument

    "test basic ast structure for help" in {
      val List(methodNode) = cpg.method.name("help").l
      methodNode.signature shouldBe "main.help()"

      val List(personCall) = cpg.call.name("Person").l
      personCall.signature shouldBe "main.Person(ANY, string)"

      val List(arg1, arg2) = personCall.argument.l
      arg1.code shouldBe "Hello"
      arg2.code shouldBe "\"hi\""
    }

    "test basic ast structure for Person" in {
      val List(methodNode) = cpg.method.name("Person").l
      methodNode.signature shouldBe "main.Person(ANY, string)"

      val List(callNode) = methodNode.call.l
      callNode.code shouldBe "hello(value)"
      callNode.signature shouldBe "main.hello()"
    }
  }
  // TODO: add unit test for "sem chan int"
  //        resultErrChan := make(chan error)
  //		sem := make(chan int, concurrency)
  // As well as example of "map"
  // TODO: Add unit tests for lambda expression as a parameter
  // TODO: Add unit test for tuple return
}
