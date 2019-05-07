package io.shiftleft.joern

import io.shiftleft.cpgqueryingtests.codepropertygraph.{CpgFactory, LanguageFrontend}
import org.scalatest.{Matchers, WordSpec}

class MethodTests extends WordSpec with Matchers {

  val cpgFactory = new CpgFactory(LanguageFrontend.Fuzzyc, "src/main/resources/default.semantics")
  val cpg = cpgFactory.buildCpg(
    """
      int main(int argc, char **argv) { }
    """.stripMargin
  )

  "should return correct function/method name" in {
    cpg.method.name.toSet shouldBe Set("main")
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


}
