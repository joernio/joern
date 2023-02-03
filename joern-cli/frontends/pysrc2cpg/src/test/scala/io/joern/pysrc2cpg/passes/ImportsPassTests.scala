package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language._

class ImportsPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  // These first tests describe the state the graph should be in
  // before the pass is executed.

  "For a simple import statement, there" should {

    lazy val cpg = code("import foo")

    "be a create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(, foo)"
      val List(lit1, lit2) = callToImport.argument.l
      lit1.code shouldBe ""
      lit2.code shouldBe "foo"
    }

    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "foo"
      assignment.source.code shouldBe "import(, foo)"
    }
  }

  "For an import of the form `from... import`, it" should {

    lazy val cpg = code("from foo import Bar")

    "create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(foo, Bar)"
      val List(lit1, lit2) = callToImport.argument.l
      lit1.code shouldBe "foo"
      lit2.code shouldBe "Bar"
    }

    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "Bar"
      assignment.source.code shouldBe "import(foo, Bar)"
    }

  }

}
