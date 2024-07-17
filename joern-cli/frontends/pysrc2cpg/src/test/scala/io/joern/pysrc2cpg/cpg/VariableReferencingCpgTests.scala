package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VariableReferencingCpgTests extends AnyFreeSpec with Matchers {
  "local variable reference" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def f():
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.closureBindingId shouldBe None
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.referencingIdentifiers.lineNumber(2).code.head shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(3).code.head shouldBe "x"
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").head
      f.lineNumber shouldBe Some(1)
      f.columnNumber shouldBe Some(1)
      val x = cpg.method.name("f").local.nameExact("x").head
      val y = cpg.method.name("f").local.nameExact("y").head
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
      val paramNode = cpg.method.name("f").parameter.name("x").head
    }

    "test identifier association to local" in {
      val paramNode = cpg.method.name("f").parameter.name("x").head
      paramNode.referencingIdentifiers.lineNumber(2).code.head shouldBe "x"
      paramNode.referencingIdentifiers.lineNumber(3).code.head shouldBe "x"
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").head
      f.lineNumber shouldBe Some(1)
      f.columnNumber shouldBe Some(1)

      val x = cpg.method.name("f").parameter.name("x").head
      val y = cpg.method.name("f").local.name("y").head
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
      cpg.method.name("<module>").block.local.name("x").head
    }

    "test local variable exists in comprehension block" in {
      cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).head
    }

    "test identifiers in line 1 and 3 reference to module method x variable" in {
      val localXNode = cpg.method.name("<module>").block.local.name("x").head
      cpg.identifier("x").lineNumber(1).refsTo.head shouldBe localXNode
      cpg.identifier("x").lineNumber(3).refsTo.head shouldBe localXNode
    }

    "test identifiers in line 2 reference to comprehension block x variable" in {
      val localXNode = cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).head
      cpg.identifier("x").lineNumber(2).refsTo.foreach(local => local shouldBe localXNode)
    }

    "test local variable line and column numbers" in {
      val f = cpg.local.nameExact("f").head
      f.lineNumber shouldBe Some(3)
      f.columnNumber shouldBe Some(1)

      val x = cpg.local("x").filterNot(_.definingBlock.astParent.isMethod.isEmpty).head
      val y = cpg.local("y").filterNot(_.definingBlock.astParent.isMethod.isEmpty).head
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
      cpg.method.name("<module>").block.local.name("x").head
    }

    "test local variable exists in comprehension block" in {
      cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).head
    }

    "test identifiers in line 1 and 3 reference to module method x variable" in {
      val localXNode = cpg.method.name("<module>").block.local.name("x").head
      cpg.identifier("x").lineNumber(1).refsTo.head shouldBe localXNode
      cpg.identifier("x").lineNumber(3).refsTo.head shouldBe localXNode
    }

    "test identifiers in line 2 reference to comprehension block x variable" in {
      val localXNode = cpg.local("x").filter(_.definingBlock.astParent.isMethod.isEmpty).head
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
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.closureBindingId shouldBe Some("test.py:<module>.f:x")
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.referencingIdentifiers.lineNumber(4).code.head shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(5).code.head shouldBe "x"
    }

    "test method reference closure binding" in {
      val methodRefNode  = cpg.methodRefWithName("f").head
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.f:x")
    }
  }

  "global variable (implicitly created) reference " - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def f():
        |  global x
        |  x = 1
        |  y = x""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.closureBindingId shouldBe Some("test.py:<module>.f:x")
    }

    "test identifier association to local" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.referencingIdentifiers.lineNumber(3).code.head shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(4).code.head shouldBe "x"
    }

    "test method reference closure binding" in {
      val methodRefNode  = cpg.methodRefWithName("f").head
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
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
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.closureBindingId shouldBe Some("test.py:<module>.g.f:x")
    }

    "test identifier association to local in f" in {
      val localNode = cpg.method.name("f").local.name("x").head
      localNode.referencingIdentifiers.lineNumber(5).code.head shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(6).code.head shouldBe "x"
    }

    "test method reference closure binding of f in g" in {
      val methodRefNode  = cpg.methodRefWithName("f").head
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.g.f:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test local variable exists in g" in {
      val localNode = cpg.method.name("g").local.name("x").head
      localNode.closureBindingId shouldBe Some("test.py:<module>.g:x")
    }

    "test identifier association to local in g" in {
      val localNode = cpg.method.name("g").local.name("x").head
    }

    "test method reference closure binding of g in module" in {
      val methodRefNode  = cpg.methodRefWithName("g").head
      val closureBinding = methodRefNode._closureBindingViaCaptureOut.next()
      closureBinding.closureBindingId shouldBe Some("test.py:<module>.g:x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.closureOriginalName shouldBe Some("x")
    }

    "test global variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode.closureBindingId shouldBe None
    }

    "test closure binding reference to global" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.g:x")
    }
  }

  "reference from class method" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |class MyClass():
        |  x = 1
        |  def f(self):
        |    someFunc(x)
        |""".stripMargin)

    "test capturing to global x exists" in {
      val moduleLocal = cpg.method.name("<module>").local.name("x").head
      moduleLocal._closureBindingViaRefIn.next().closureBindingId shouldBe Some("test.py:<module>.MyClass.f:x")

      val bodyLocal = cpg.method.fullName("test.py:<module>.MyClass.<body>").local.name("x").head
      bodyLocal.closureBindingId shouldBe None

      val fLocal = cpg.method.fullName("test.py:<module>.MyClass.f").local.name("x").head
      fLocal.closureBindingId shouldBe Some("test.py:<module>.MyClass.f:x")

    }
  }

  "reference from class body method" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 0
        |class MyClass():
        |  x = 1
        |  someFunc(x)
        |""".stripMargin)

    "test reference to body local variable exists" in {
      val localNode = cpg.method.name("<module>").local.name("x").head
      localNode._closureBindingViaRefIn.hasNext shouldBe false

      val localInMyClassNode = cpg.method.name("<body>").local.name("x").head
      localInMyClassNode.referencingIdentifiers.lineNumber(4).code.head shouldBe "x"
    }
  }

  "delete wildcard imported variable" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""from foo import *
        |del someImportedVariable
        |""".stripMargin)

    "test local variable exists in module method" in {
      val localNode = cpg.method.name("<module>").local.name("someImportedVariable").head
    }

    "test reference to local variable" in {
      val localNode        = cpg.method.name("<module>").local.name("someImportedVariable").head
      val List(identifier) = localNode.referencingIdentifiers.l
      identifier.name shouldBe "someImportedVariable"
    }
  }
}
