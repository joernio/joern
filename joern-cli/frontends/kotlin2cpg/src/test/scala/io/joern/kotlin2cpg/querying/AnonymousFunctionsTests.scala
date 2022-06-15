package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

@Ignore
class AnonymousFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver = NoResolve

  "CPG for code with anonymous function as argument" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |import kotlin.collections.List
        |import kotlin.collections.listOf
        |
        |fun foo(x: String): Int {
        |    val l: kotlin.collections.List = listOf(1, x)
        |    l.filter(fun(item) = { println(item); item > 0 })
        |    return 0
        |}
        |""".stripMargin)

    // TODO: add tests
  }
}
