package io.shiftleft.joern

import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

class SampleDocuTest extends WordSpec with Matchers {

  val code = """
       int main(int argc, char **argv) { }
    """

  new TestCpg().buildCpg(code) { cpg =>
    "should return `main` as the only method" in {
      cpg.method.name.toSet shouldBe Set("main")
    }
  }

}
