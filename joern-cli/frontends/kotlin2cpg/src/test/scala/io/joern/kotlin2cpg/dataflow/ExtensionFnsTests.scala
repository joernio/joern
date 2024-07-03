package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ExtensionFnsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with extension fn using `this` inside its body" should {
    val cpg = code("""
      |package mypkg
      |open class AClass(val p1: String)
      |fun AClass.doSomething() {
      |    println(this.p1)
      |}
      |fun f1(p: String) {
      |    val a = AClass(p)
      |    a.doSomething()
      |}
      |""".stripMargin)

    "find a flow for through the usage of `this`" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").parameter
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(7)),
            ("AClass(p)", Some(8)),
            ("<init>(this, p1)", Some(3)),
            ("this.p1 = p1", Some(3)),
            ("this.p1 = p1", None),
            ("RET", None),
            ("AClass(p)", Some(8)),
            ("a.doSomething()", Some(9)),
            ("doSomething(this)", None),
            ("println(this.p1)", Some(5)),
            ("println(p1)", None)
          )
        )
    }
  }
}
