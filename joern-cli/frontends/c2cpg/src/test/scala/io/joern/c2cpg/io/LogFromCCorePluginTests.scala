package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import org.eclipse.cdt.core.CCorePlugin
import org.eclipse.cdt.internal.core.parser.ParserException

class LogFromCCorePluginTests extends C2CpgSuite {

  private val codeString = """
   |// A comment
   |int my_func(int param1)
   |{
   |   int x = foo(param1);
   |}""".stripMargin

  private val cpg = code(codeString)

  "logging from CCorePlugin" should {

    "not crash with an exception" in {
      noException should be thrownBy CCorePlugin.log(new ParserException("Test Exception!"))
      val List(func) = cpg.method.nameExact("my_func").l
      func.fullName shouldBe "my_func"
    }
  }

}
