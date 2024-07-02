package io.joern.kotlin2cpg.compiler

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class JavaInteroperabilityTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with Java interop" should {
    val cpg = code(
      """package no.such.pkg
        |fun someFun(x: String)
        |fun main() {
        |  val c = SomeJavaClass()
        |  c.someFunction("greetings")
        |}
        |""".stripMargin,
      "App.kt"
    )
      .moreCode(
        """package no.such.pkg;
          |public class SomeJavaClass {
          |    public void someFunction(String text) {
          |      System.out.println(text);
          |    }
          |}
          |""".stripMargin,
        "SomeJavaClass.java"
      )

    "should contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code("c.someFunction.*").l
      c.methodFullName shouldBe "no.such.pkg.SomeJavaClass.someFunction:void(java.lang.String)"
    }

    "should contain a CALL node for the call found inside the file written in Java" in {
      val List(c) = cpg.call.codeExact("System.out.println(text)").l
      c.methodFullName shouldBe "java.io.PrintStream.println:void(java.lang.String)"
    }

    "should deduplicate types if found in both Java and Kotlin code" in {
      cpg.typ.fullNameExact("java.lang.String").size shouldBe 1
    }
  }
}
