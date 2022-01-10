package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.jIteratortoTraversal

class DelegatedPropertiesTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple delegated properties" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class MyClass {
        |  val myName: String by lazy { "one" + "two" }
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node with a corresponding MEMBER" in {
      cpg.typeDecl
        .fullName("mypkg.MyClass")
        .member
        .size should not be 0
    }
  }
}
