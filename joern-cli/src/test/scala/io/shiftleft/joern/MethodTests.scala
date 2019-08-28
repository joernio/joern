package io.shiftleft.joern

import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

/**
  * Language primitives for navigating method stubs
  * */
class MethodTests extends WordSpec with Matchers {

  val code = """
       int main(int argc, char **argv) { }
    """

  "should return correct function/method name" in {
    new TestCpg().buildCpg(code) { cpg =>
      cpg.method.name.toSet shouldBe Set("main")
    }
  }

  "should have correct type signature" in
    new TestCpg().buildCpg(code) { cpg =>
      cpg.method.signature.toSet shouldBe Set("int(int,char * *)")
    }

  "should return correct number of parameters" in
    new TestCpg().buildCpg(code) { cpg =>
      cpg.parameter.name.toSet shouldBe Set("argc", "argv")
      cpg.method.name("main").parameter.name.toSet shouldBe Set("argc", "argv")
    }

  "should return correct parameter types" in
    new TestCpg().buildCpg(code) { cpg =>
      cpg.parameter.name("argc").evalType.l shouldBe List("int")
      cpg.parameter.name("argv").evalType.l shouldBe List("char * *")
    }

  "should return correct return type" in
    new TestCpg().buildCpg(code) { cpg =>
      cpg.methodReturn.evalType.l shouldBe List("int")
      cpg.method.name("main").methodReturn.evalType.l shouldBe List("int")
      cpg.parameter.name("argc").method.methodReturn.evalType.l shouldBe List("int")
    }

  "should return a filename for method 'main'" in
    new TestCpg().buildCpg(code) { cpg =>
      cpg.method.name("main").file.name.l should not be empty
    }

}
