package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}

class SampleDocuTest extends WordSpec with Matchers {

  new TestCpg(
    """
       int main(int argc, char **argv) { }
    """
  ) {
    "should return `main` as the only method" in {
      cpg.method.name.toSet shouldBe Set("main")
    }
  }

}
