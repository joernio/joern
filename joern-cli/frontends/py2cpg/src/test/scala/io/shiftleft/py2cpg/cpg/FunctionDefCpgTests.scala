package io.shiftleft.py2cpg.cpg

import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FunctionDefCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg(
    """def func(a, b):
      |  pass
      |""".stripMargin
  )

  "foo" ignore {
    // TODO
  }

}
