package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LambdaTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code containing a lambda with parameter destructuring" should {
    val cpg = code("""
         |package mypkg
         |
         |fun f1(p: String) {
         |    val m = mapOf(p to 1, "two" to 2, "three" to 3)
         |    m.forEach { (k, v) ->  println(k) }
         |}
         |""".stripMargin)

    "find a flow we want" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(4)),
            ("tmp_1 = it", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("k = tmp_1.component1()", Some(6)),
            ("println(k)", Some(6))
          )
        )
    }
  }

  "CPG for code containing a lambda with parameter destructuring and shadowing" should {
    val cpg = code("""
         |package mypkg
         |
         |fun f1(p: String) {
         |    val m = mapOf(p to 1, "two" to 2)
         |    m.forEach { (p, _) ->  println(p) }
         |}
         |""".stripMargin)

    "find a value flow from outer parameter to shadowed lambda sink through destructuring" in {
      val source = cpg.method.name("f1").parameter.nameExact("p")
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(4)),
            ("tmp_1 = it", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("p = tmp_1.component1()", Some(6)),
            ("println(p)", Some(6))
          )
        )
    }
  }

  "CPG for code containing a lambda with parameter destructuring and underscore entry" should {
    val cpg = code("""
         |package mypkg
         |
         |fun f1(p: String) {
         |    val m = mapOf(p to 1, "two" to 2, "three" to 3)
         |    m.forEach { (k, _) -> println(k) }
         |}
         |""".stripMargin)

    "find a flow through component1 but not component2" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(4)),
            ("tmp_1 = it", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("k = tmp_1.component1()", Some(6)),
            ("println(k)", Some(6))
          )
        )
    }
  }

  "CPG for code containing a lambda with parameter destructuring on a safe-qualified receiver" should {
    val cpg = code("""
         |package mypkg
         |
         |fun f1(p: String) {
         |    val m: Map<String, Int>? = mapOf(p to 1, "two" to 2)
         |    m?.forEach { (k, _) -> println(k) }
         |}
         |""".stripMargin)

    "find a flow from outer parameter to sink through safe-qualified destructuring" in {
      val source = cpg.method.name("f1").parameter.nameExact("p")
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(4)),
            ("tmp_1 = it", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("k = tmp_1.component1()", Some(6)),
            ("println(k)", Some(6))
          )
        )
    }
  }

  "CPG for code containing a lambda with destructuring sourced from an extension-receiver alias" should {
    val cpg = code("""
         |package mypkg
         |
         |class C {
         |    fun Map<String, Int>.f1() {
         |        val receiverAlias = this
         |        receiverAlias.forEach { (k, _) -> println(k) }
         |    }
         |}
         |""".stripMargin)

    "find a flow from extension receiver to sink through alias and destructuring" in {
      val source = cpg.method.name("f1").parameter.nameExact("this")
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(this)", Some(5)),
            ("tmp_1 = it", Some(7)),
            ("tmp_1.component1()", Some(7)),
            ("k = tmp_1.component1()", Some(7)),
            ("println(k)", Some(7))
          )
        )
    }
  }
}
