package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

@Ignore
class AnonymousFunctionsTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with anonymous function as argument" - {
    lazy val cpg = TestContext.buildCpg("""
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
