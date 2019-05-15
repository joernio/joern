package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}

/**
  * Language primitives for navigating method stubs
  * */
class MethodTests extends WordSpec with Matchers {

  new TestCpg(
    """
       int main(int argc, char **argv) { }
    """
  ) {
    "should return correct function/method name" in {
      cpg.method.name.toSet shouldBe Set("main")
    }

    "should have correct type signature" in {
      cpg.method.signature.toSet shouldBe Set("int(int,char * *)")
    }

    "should return correct number of parameters" in {
      cpg.parameter.name.toSet shouldBe Set("argc", "argv")
      cpg.method.name("main").parameter.name.toSet shouldBe Set("argc", "argv")
    }

    "should return correct parameter types" in {
      cpg.parameter.name("argc").evalType.l shouldBe List("int")
      cpg.parameter.name("argv").evalType.l shouldBe List("char * *")
    }

    "should return correct return type" in {
      cpg.methodReturn.evalType.l shouldBe List("int")
      cpg.method.name("main").methodReturn.evalType.l shouldBe List("int")
      cpg.parameter.name("argc").method.methodReturn.evalType.l shouldBe List("int")
    }

    "should return a filename for method 'main'" in {
      cpg.method.name("main").file.name.l should not be empty
    }
  }

}
