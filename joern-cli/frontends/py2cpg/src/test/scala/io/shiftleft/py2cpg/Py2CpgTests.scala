package io.shiftleft.py2cpg

import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Py2CpgTests extends AnyWordSpec with Matchers {
  "foo" in {
    val cpg = Py2CpgTest.newContext.buildCpg("""print("Foo")""")

    val x = cpg.literal.l
    println(x)
  }

}
