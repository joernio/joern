package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DotQualifiedExpressionsTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with dot qualified expression as a receiver" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import io.javalin.Javalin
        |
        |import kotlin.io.println
        |
        |fun main(args : Array<String>) {
        |  val app = Javalin.create().start(7000)
        |  app.get("/status") { ctx -> ctx.result("ok") }
        |}
        |""".stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `Javalin.create.*` with the correct properties set" in {
      val List(c) = cpg.call.code("Javalin.create.*7000.*").l
      c.methodFullName shouldBe "io.javalin.Javalin.start:io.javalin.Javalin(java.lang.Integer)"
      c.lineNumber shouldBe Some(8)
      c.columnNumber shouldBe Some(12)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }
}
