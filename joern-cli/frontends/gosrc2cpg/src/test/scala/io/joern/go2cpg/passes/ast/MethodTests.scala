package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.*

import java.io.File

class MethodTests extends GoCodeToCpgSuite {

  "Empty parameters" should {
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
      x.signature shouldBe "main.foo ()"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "check binding Node" in {
      val List(x) = cpg.method.name("foo").bindingTypeDecl.l
      x.name shouldBe "main.<global>"
      x.fullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, int, string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, int, []string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, int, []int)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, *string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, []*string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, []*string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, *[]string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
      x.signature shouldBe "main.foo (int, main.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, main.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
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
      x.signature shouldBe "joern.io/sample.foo (int, joern.io/sample.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:joern.io/sample.<global>"
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
      x.signature shouldBe "main.foo (int, joern.io/sample/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, []joern.io/sample/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, []privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, *privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, []*privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, *[]privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (int, []*privado.ai/test/fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
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
      x.signature shouldBe "main.foo (any, any, []any, *any, []any)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
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
  // TODO: Add pointer to pointer use case.
  // TODO: Add unit test for tuple return
  // TODO: Add unit tests with Generics
}
