package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class IndexAccessTests extends RubyCode2CpgFixture {

  "`x[1]` is represented by an `indexAccess` operator call" in {
    val cpg = code("""x = Array()
                     |x[1]
                     |""".stripMargin)

    val List(indexAccess) = cpg.call(Operators.indexAccess).l

    indexAccess.methodFullName shouldBe Operators.indexAccess
    indexAccess.code shouldBe "x[1]"
    indexAccess.lineNumber shouldBe Some(2)
    indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(x)   = indexAccess.argument.order(1).l
    val List(one) = indexAccess.argument.order(2).l

    x.code shouldBe "x"
    x.lineNumber shouldBe Some(2)

    one.code shouldBe "1"
    one.lineNumber shouldBe Some(2)
  }

  "`x[1,2]` is represented by an `indexAccess` operator call" in {
    val cpg = code("""x = Array()
        |x[1,2]
        |""".stripMargin)

    val List(indexAccess) = cpg.call(Operators.indexAccess).l
    indexAccess.methodFullName shouldBe Operators.indexAccess
    indexAccess.code shouldBe "x[1,2]"
    indexAccess.lineNumber shouldBe Some(2)

    val List(x)   = indexAccess.argument.order(1).l
    val List(one) = indexAccess.argument.order(2).l
    val List(two) = indexAccess.argument.order(3).l

    x.code shouldBe "x"
    one.code shouldBe "1"
    two.code shouldBe "2"
  }

}
