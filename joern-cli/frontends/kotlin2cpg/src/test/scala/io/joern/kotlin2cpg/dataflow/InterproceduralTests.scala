package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class InterproceduralTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with two simple functions of the same package defined in separate files" should {
    val cpg = code(
      """
      |package mypkg
      |fun f1() {
      |  val aVal = 41414141
      |  f2(aVal)
      |}
      |""".stripMargin,
      "file1.kt"
    )
      .moreCode(
        """
        |package mypkg
        |fun f2(p: Int) {
        |  println(p)
        |}
        |""".stripMargin,
        "file2.kt"
      )

    "find flows through the function" in {
      val source = cpg.literal.codeExact("41414141")
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("val aVal = 41414141", Some(4)), ("f2(aVal)", Some(5)), ("f2(p)", Some(3)), ("println(p)", Some(4))))
    }
  }

  "CPG for code with two simple classes of the same package defined in separate files" should {
    val cpg = code(
      """
       |package mypkg
       |fun f1() {
       |  val aVal = 41414141
       |  f2(aVal)
       |}
       |
       |fun f2(p: Int) {
       |  f3(p)
       |}
       |""".stripMargin,
      "file1.kt"
    )
      .moreCode(
        """
         |package mypkg
         |fun f3(p: Int) {
         |  println(p)
         |}
         |""".stripMargin,
        "file2.kt"
      )

    "find flows through the function" in {
      val source = cpg.literal.codeExact("41414141")
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("val aVal = 41414141", Some(4)),
            ("f2(aVal)", Some(5)),
            ("f2(p)", Some(8)),
            ("f3(p)", Some(9)),
            ("f3(p)", Some(3)),
            ("println(p)", Some(4))
          )
        )
    }
  }
}
