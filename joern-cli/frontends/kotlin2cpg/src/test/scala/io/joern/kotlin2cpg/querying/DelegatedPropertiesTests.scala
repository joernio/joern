package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DelegatedPropertiesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple delegated properties" should {
    val cpg = code("""
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
