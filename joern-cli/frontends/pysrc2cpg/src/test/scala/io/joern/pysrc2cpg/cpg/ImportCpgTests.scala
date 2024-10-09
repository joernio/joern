package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ImportCpgTests extends AnyFreeSpec with Matchers {

  "test plain import statement" in {
    lazy val cpg   = Py2CpgTestContext.buildCpg("""import a""".stripMargin)
    val assignment = cpg.call.code(".*=.*import.*").l

    val lhsIdentifier = assignment.argument(1).isIdentifier.head
    lhsIdentifier.code shouldBe "a"

    val fromLiteral = assignment.argument(2).isCall.argument(1).head
    fromLiteral.code shouldBe ""

    val importedEntity = assignment.argument(2).isCall.argument(2).head
    importedEntity.code shouldBe "a"
  }

  "test plain import statement with hierarchical name" in {
    lazy val cpg   = Py2CpgTestContext.buildCpg("""import a.b""".stripMargin)
    val assignment = cpg.call.code(".*=.*import.*").l

    val lhsIdentifier = assignment.argument(1).isIdentifier.head
    lhsIdentifier.code shouldBe "a"

    val fromLiteral = assignment.argument(2).isCall.argument(1).head
    fromLiteral.code shouldBe ""

    val importedEntity = assignment.argument(2).isCall.argument(2).head
    importedEntity.code shouldBe "a.b"
  }

  "test 'from ... import' statement with hierarchical name" in {
    lazy val cpg   = Py2CpgTestContext.buildCpg("""from a.b import c""".stripMargin)
    val assignment = cpg.call.code(".*=.*import.*").l

    val lhsIdentifier = assignment.argument(1).isIdentifier.head
    lhsIdentifier.code shouldBe "c"

    val fromLiteral = assignment.argument(2).isCall.argument(1).head
    fromLiteral.code shouldBe "a.b"

    val importedEntity = assignment.argument(2).isCall.argument(2).head
    importedEntity.code shouldBe "c"
  }

}
