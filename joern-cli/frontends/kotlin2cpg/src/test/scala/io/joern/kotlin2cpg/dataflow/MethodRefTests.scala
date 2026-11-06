package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodRefTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for a member call given a method reference" should {
    val cpg = code("""
        |package mypkg
        |
        |class C {
        |    fun run(f: (String) -> Unit) {}
        |}
        |
        |fun consume(p: String) {
        |    println(p)
        |}
        |
        |fun main(c: C) {
        |    c.run(::consume)
        |}
        |""".stripMargin)

    "follow the call receiver into the referenced method" in {
      val source = cpg.method.name("main").parameter.nameExact("c")
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.size should be > 0
    }
  }

  "CPG for an extension call given a method reference" should {
    val cpg = code("""
        |package mypkg
        |
        |fun consume(p: String) {
        |    println(p)
        |}
        |
        |fun main(src: String) {
        |    listOf(src).forEach(::consume)
        |}
        |""".stripMargin)

    "follow the extended value into the referenced method" in {
      val source = cpg.method.name("main").parameter.nameExact("src")
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.size should be > 0
    }
  }

  "CPG for a static call given a method reference" should {
    val cpg = code("""
        |package mypkg
        |
        |fun cmp(left: String, right: String): Int {
        |    println(left)
        |    return 0
        |}
        |
        |fun main(src: String) {
        |    val xs = mutableListOf(src)
        |    java.util.Collections.sort(xs, ::cmp)
        |}
        |""".stripMargin)

    "follow the sorted collection into the referenced method" in {
      val source = cpg.method.name("main").parameter.nameExact("src")
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.size should be > 0
    }
  }
}
