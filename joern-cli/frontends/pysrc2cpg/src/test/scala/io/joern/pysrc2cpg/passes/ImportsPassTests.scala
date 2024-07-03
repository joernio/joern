package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportsPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "For a simple import statement, there" should {
    lazy val cpg = code("import foo")
    "be a create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(, foo)"
      val List(where, what) = callToImport.argument.l
      where.code shouldBe ""
      what.code shouldBe "foo"
    }
    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "foo"
      assignment.source.code shouldBe "import(, foo)"
    }

    "create an IMPORT node" in {
      val List(importNode) = cpg.imports.l
      importNode.importedEntity shouldBe Some("foo")
      importNode.importedAs shouldBe Some("foo")
    }

  }

  "For an import of the form `from... import`, it" should {
    lazy val cpg = code("from foo import Bar")
    "create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(foo, Bar)"
      val List(where, what) = callToImport.argument.l
      where.code shouldBe "foo"
      what.code shouldBe "Bar"
    }

    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "Bar"
      assignment.source.code shouldBe "import(foo, Bar)"
    }

    "create an IMPORT node" in {
      val List(importNode) = cpg.imports.l
      importNode.importedEntity shouldBe Some("foo.Bar")
      importNode.importedAs shouldBe Some("Bar")
    }

  }

  "For an import of a module with alias, it" should {
    lazy val cpg = code("import foo as bar")
    "create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(, foo, bar)"
      val List(where, what, as) = callToImport.argument.l
      where.code shouldBe ""
      what.code shouldBe "foo"
      as.code shouldBe "bar"
    }

    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "bar"
      assignment.source.code shouldBe "import(, foo, bar)"
    }

    "create an IMPORT node" in {
      val List(importNode) = cpg.imports.l
      importNode.importedEntity shouldBe Some("foo")
      importNode.importedAs shouldBe Some("bar")
    }

  }

  "For an import of a class by alias, it" should {
    lazy val cpg = code("from foo import Bar as Woo")
    "create a call to `import`" in {
      val List(callToImport) = cpg.call("import").l
      callToImport.code shouldBe "import(foo, Bar, Woo)"
      val List(where, what, as) = callToImport.argument.l
      where.code shouldBe "foo"
      what.code shouldBe "Bar"
      as.code shouldBe "Woo"
    }

    "create an assignment with the import on the right-hand-side" in {
      val List(assignment) = cpg.call("import").inAssignment.l
      assignment.target.code shouldBe "Woo"
      assignment.source.code shouldBe "import(foo, Bar, Woo)"
    }

    "create an IMPORT node" in {
      val List(importNode) = cpg.imports.l
      importNode.importedEntity shouldBe Some("foo.Bar")
      importNode.importedAs shouldBe Some("Woo")
    }

  }

}
