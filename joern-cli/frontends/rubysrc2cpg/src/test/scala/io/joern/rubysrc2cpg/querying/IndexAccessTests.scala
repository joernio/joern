package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class IndexAccessTests extends RubyCode2CpgFixture {

  "`x[1]` is represented by an `indexAccess` operator call" ignore {
    val cpg = code("""
                     |x[1]
                     |""".stripMargin)

    val List(indexAccess) = cpg.call(Operators.indexAccess).l

    indexAccess.methodFullName shouldBe Operators.indexAccess
    indexAccess.code shouldBe "x[1]"
    indexAccess.lineNumber shouldBe Some(2)

    val List(one) = indexAccess.argument.l
    one.code shouldBe "1"
    one.lineNumber shouldBe Some(2)
  }

}
