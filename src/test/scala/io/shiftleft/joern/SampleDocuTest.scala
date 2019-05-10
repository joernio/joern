package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}

class SampleDocuTest extends WordSpec with Matchers {
  val code =
    """
       int main(int argc, char **argv) { }
    """
  val cpg = createTestCpg(code)

  "should return `main` as the only method" in {
    cpg.method.name.toSet shouldBe Set("main")
  }
}
