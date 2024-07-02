package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class JavaInteroperabilityTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with Java interop" should {
    val cpg = code(
      """package no.such.pkg
        |fun main() {
        |  val c = SomeJavaClass()
        |  val someGreeting = "hithere"
        |  c.someFunction(someGreeting)
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

    "should find a flow from an IDENTIFIER defined in Kotlin code to the argument of a CALL inside Java code" in {
      val source = cpg.local.nameExact("someGreeting").referencingIdentifiers
      val sink   = cpg.call.code("System.out.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("val someGreeting = \"hithere\"", Some(4)),
            ("c.someFunction(someGreeting)", Some(5)),
            ("someFunction(this, String text)", Some(3)),
            ("System.out.println(text)", Some(4))
          ),
          List(
            ("c.someFunction(someGreeting)", Some(5)),
            ("someFunction(this, String text)", Some(3)),
            ("System.out.println(text)", Some(4))
          )
        )
    }
  }
}
