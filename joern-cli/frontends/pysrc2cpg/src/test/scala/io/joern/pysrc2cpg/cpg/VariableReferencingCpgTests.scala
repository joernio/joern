package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VariableReferencingCpgTests extends AnyFreeSpec with Matchers {
  "local variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def f():
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.closureBindingId shouldBe None
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.referencingIdentifiers.lineNumber(2).code.next() shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(3).code.next() shouldBe "x"
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").next()
      f.lineNumber shouldBe Some(1)
      f.columnNumber shouldBe Some(1)
      val x = cpg.method.name("f").local.nameExact("x").next()
      val y = cpg.method.name("f").local.nameExact("y").next()
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(3)
      y.lineNumber shouldBe Some(3)
      y.columnNumber shouldBe Some(3)
    }

  }

  "parameter variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def f(x):
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val paramNode = cpg.method.name("f").parameter.name("x").next()
    }

    "test identifier association to local" in {
      val paramNode = cpg.method.name("f").parameter.name("x").next()
      paramNode.referencingIdentifiers.lineNumber(2).code.next() shouldBe "x"
      paramNode.referencingIdentifiers.lineNumber(3).code.next() shouldBe "x"
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").next()
      f.lineNumber shouldBe Some(1)
      f.columnNumber shouldBe Some(1)

      val x = cpg.method.name("f").parameter.name("x").next()
      val y = cpg.method.name("f").local.name("y").next()
      x.lineNumber shouldBe Some(1)
      x.columnNumber shouldBe Some(7)
      y.lineNumber shouldBe Some(3)
      y.columnNumber shouldBe Some(3)
    }

  }

  "comprehension variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 1
        |[x for x in y]
        |f(x)""".stripMargin)

    "test local variable exists in module method" in {
      cpg.method.name("<module>").block.local.name("x").next()
    }

    "test local variable exists in comprehension block" in {
      cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).next()
    }

    "test identifiers in line 1 and 3 reference to module method x variable" in {
      val localXNode = cpg.method.name("<module>").block.local.name("x").next()
      cpg.identifier("x").lineNumber(1).refsTo.next() shouldBe localXNode
      cpg.identifier("x").lineNumber(3).refsTo.next() shouldBe localXNode
    }

    "test identifiers in line 2 reference to comprehension block x variable" in {
      val localXNode = cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).next()
      cpg.identifier("x").lineNumber(2).refsTo.foreach(local => local shouldBe localXNode)
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").next()
      f.lineNumber shouldBe Some(3)
      f.columnNumber shouldBe Some(1)

      val x = cpg.local("x").filterNot(_.definingBlock.astParent.isMethod.isEmpty).next()
      val y = cpg.local("y").filterNot(_.definingBlock.astParent.isMethod.isEmpty).next()
      x.lineNumber shouldBe Some(1)
      x.columnNumber shouldBe Some(1)
      y.lineNumber shouldBe Some(2)
      y.columnNumber shouldBe Some(13)
    }
  }

  "comprehension tuple variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 1
        |[x for (x,) in y]
        |f(x)""".stripMargin)

    "test local variable exists in module method" in {
      cpg.method.name("<module>").block.local.name("x").next()
    }

    "test local variable exists in comprehension block" in {
      cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).next()
    }

    "test identifiers in line 1 and 3 reference to module method x variable" in {
      val localXNode = cpg.method.name("<module>").block.local.name("x").next()
      cpg.identifier("x").lineNumber(1).refsTo.next() shouldBe localXNode
      cpg.identifier("x").lineNumber(3).refsTo.next() shouldBe localXNode
    }

    "test identifiers in line 2 reference to comprehension block x variable" in {
      val localXNode = cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).next()
      cpg.identifier("x").lineNumber(2).refsTo.foreach(local => local shouldBe localXNode)
    }
  }

  "global variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |def f():
        |  global x
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.closureBindingId shouldBe Some("test.py:<module>.f:x")
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.referencingIdentifiers.lineNumber(4).code.next() shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(5).code.next() shouldBe "x"
    }

    "test method reference closure binding" in {
      val methodRefNode  = cpg.methodRef("f").next()
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.f:x")
    }
  }

  "global variable (implicitly created) reference " - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def f():
        |  global x
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.closureBindingId shouldBe Some("test.py:<module>.f:x")
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.referencingIdentifiers.lineNumber(3).code.next() shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(4).code.next() shouldBe "x"
    }

    "test method reference closure binding" in {
      val methodRefNode  = cpg.methodRef("f").next()
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.f:x")
    }
  }

  "nested global variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |def g():
        |  def f():
        |    global x
        |    x = 1
        |    y = x""".stripMargin)

    "test local variable exists in f" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.closureBindingId shouldBe Some("test.py:<module>.g.f:x")
    }

    "test identifier association to local in f" in {
      val localNode = cpg.method.name("f").local.name("x").next()
      localNode.referencingIdentifiers.lineNumber(5).code.next() shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(6).code.next() shouldBe "x"
    }

    "test method reference closure binding of f in g" in {
      val methodRefNode  = cpg.methodRef("f").next()
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.g.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test local variable exists in g" in {
      val localNode = cpg.method.name("g").local.name("x").next()
      localNode.closureBindingId shouldBe Some("test.py:<module>.g:x")
    }

    "test identifier association to local in g" in {
      val localNode = cpg.method.name("g").local.name("x").next()
    }

    "test method reference closure binding of g in module" in {
      val methodRefNode  = cpg.methodRef("g").next()
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.g:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.g:x")
    }
  }

  "reference from class method" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |class MyClass():
        |  x = 1
        |  def f():
        |    someFunc(x)
        |""".stripMargin)

    "test capturing to global x exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some(
        "test.py:<module>.MyClass.MyClass<body>.f:x"
      )

      val localInMyClassNode = cpg.method.name("MyClass<body>").local.name("x").next()
      localInMyClassNode.referencingIdentifiers.lineNumber(5).hasNext shouldBe false
    }
  }

  "reference from class body method" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |class MyClass():
        |  x = 1
        |  someFunc(x)
        |""".stripMargin)

    "test reference to body local variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").next()
      localNode._closureBindingViaRefIn.hasNext shouldBe false

      val localInMyClassNode = cpg.method.name("MyClass<body>").local.name("x").next()
      localInMyClassNode.referencingIdentifiers.lineNumber(4).code.next() shouldBe "x"
    }
  }
}
