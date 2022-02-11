package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ThisTests extends AnyFreeSpec with Matchers {
  "CPG for code with calls to functions of same name, but different scope" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun bar() { println("Top-level function") }
        |
        |class Foo {
        |    fun bar() { println("Member function") }
        |
        |    fun invokeBar(omitThis: Boolean = false)  {
        |        if (omitThis) bar() // references the top-level function
        |        else this.bar() // references the member function
        |    }
        |}
        |""".stripMargin)

    "should have at least a CALL node" in {
      cpg.call.size should not be 0
    }

    "the CALL referencing the top-level function has the correct properties" in {
      val List(x) = cpg.call.code("bar.*").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "mypkg.Foo.bar:kotlin.Unit()"
    }

    "the CALL referencing the member function has the correct properties" in {
      val List(x) = cpg.call.code("this.bar.*").l
      x.code shouldBe "this.bar()"
      x.methodFullName shouldBe "mypkg.Foo.bar:kotlin.Unit()"
    }
  }

  "CPG for code with call that has _this_ as argument" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun bar(x: Any) { println(x) }
        |
        |class Foo {
        |    fun invoke()  {
        |        bar(this)
        |    }
        |}
        |
        |
        |""".stripMargin)

    "should have a CALL node with _this_ as argument" in {
      val List(a: Identifier) = cpg.call("bar").argument.code("this").l
      a.lineNumber shouldBe Some(7)
      a.columnNumber shouldBe Some(12)
      a.typeFullName shouldBe "mypkg.Foo"
    }
  }
}
