package io.shiftleft.py2cpg

import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CodeToCpgTests extends AnyWordSpec with Matchers {
  "foo" in {
    val fixture = new Fixture("""print("Foo")""")

    val x = fixture.cpg.literal.l
    println(x)
  }

}
