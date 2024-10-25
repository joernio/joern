package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Member}
import io.shiftleft.semanticcpg.language.*

class CompanionObjectTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple unnamed companion object" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass {
        |    companion object {
        |        val m: String = "AVALUE"
        |    }
        |}
        |
        |fun main() {
        |    val out = AClass.m
        |    println(out)
        |}
        |""".stripMargin)

    "should contain two TYPE_DECL nodes with the same FULL_NAME prefix, but different NAME & FULL_NAME" in {
      val List(td1, td2) = cpg.typeDecl.fullName("mypkg.AClass.*").l
      td1.name should not be td2.name
      td1.fullName should not be td2.fullName
    }

    "should contain a TYPE_DECL node for the companion object with the correct props set" in {
      val List(td) = cpg.typeDecl.nameExact("Companion").l
      td.fullName shouldBe "mypkg.AClass$Companion"
      td.member.size shouldBe 2

      val List(firstMember: Member, secondMember: Member) = td.member.l
      firstMember.name shouldBe "m"
      firstMember.typeFullName shouldBe "java.lang.String"
      secondMember.name shouldBe Constants.companionObjectMemberName
      secondMember.typeFullName shouldBe "mypkg.AClass"
    }

    "should contain a TYPE_DECL node for the class containing the companion object with the correct props set" in {
      val List(td) = cpg.typeDecl.nameExact("AClass").l
      td.fullName shouldBe "mypkg.AClass"
    }

    "should contain a CALL node for the companion object member access with the correct props set" in {
      val List(c) = cpg.call.codeExact("AClass.m").l
      c.methodFullName shouldBe "<operator>.fieldAccess"
      c.name shouldBe "<operator>.fieldAccess"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.argument.code.l shouldBe List("AClass", "m")

      val List(firstArg: Call, secondArg: FieldIdentifier) = c.argument.l: @unchecked
      firstArg.code shouldBe "AClass"
      firstArg.typeFullName shouldBe "mypkg.AClass$Companion"
      secondArg.code shouldBe "m"
      secondArg.canonicalName shouldBe "m"

      val List(firstArgOfLoweredCall: Identifier, secondArgOfLoweredCall: FieldIdentifier) =
        firstArg.argument.l: @unchecked
      firstArgOfLoweredCall.typeFullName shouldBe "mypkg.AClass$Companion"
      firstArgOfLoweredCall.refsTo.size shouldBe 0 // yes, 0. it's how the closed-source dataflow engine wants it atm
      secondArgOfLoweredCall.canonicalName shouldBe Constants.companionObjectMemberName
    }
  }

  "nested companion object and nested class test" in {
    val cpg = code("""
                     |package mypkg
                     |
                     |class AClass {
                     |    companion object {
                     |        class BClass {
                     |            companion object NamedCompanion {
                     |            }
                     |        }
                     |    }
                     |}
                     |""".stripMargin)

    inside(cpg.typeDecl.nameExact("NamedCompanion").l) { case List(typeDecl) =>
      typeDecl.fullName shouldBe "mypkg.AClass$Companion$BClass$NamedCompanion"
    }
  }

  "CPG for code with simple named companion object" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass {
        |    companion object NamedCompanion {
        |        val m: String = "AVALUE"
        |    }
        |}
        |
        |fun main() {
        |    val out = AClass.m
        |    println(out)
        |}
        |""".stripMargin)

    "should contain two TYPE_DECL nodes with the same FULL_NAME prefix, but different NAME & FULL_NAME" in {
      val List(td1, td2) = cpg.typeDecl.fullName("mypkg.AClass.*").l
      td1.name should not be td2.name
      td1.fullName should not be td2.fullName
    }

    "should contain a TYPE_DECL node for the companion object with the correct props set" in {
      val List(td) = cpg.typeDecl.nameExact("NamedCompanion").l
      td.fullName shouldBe "mypkg.AClass$NamedCompanion"
      td.member.size shouldBe 2

      val List(firstMember: Member, secondMember: Member) = td.member.l
      firstMember.name shouldBe "m"
      firstMember.typeFullName shouldBe "java.lang.String"
      secondMember.name shouldBe Constants.companionObjectMemberName
      secondMember.typeFullName shouldBe "mypkg.AClass"
    }

    "should contain a TYPE_DECL node for the class containing the companion object with the correct props set" in {
      val List(td) = cpg.typeDecl.nameExact("AClass").l
      td.fullName shouldBe "mypkg.AClass"
    }

    "should contain a CALL node for the companion object member access with the correct props set" in {
      val List(c) = cpg.call.codeExact("AClass.m").l
      c.methodFullName shouldBe "<operator>.fieldAccess"
      c.name shouldBe "<operator>.fieldAccess"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(firstArg: Call, secondArg: FieldIdentifier) = c.argument.l: @unchecked
      firstArg.code shouldBe "AClass"
      firstArg.typeFullName shouldBe "mypkg.AClass$NamedCompanion"
      secondArg.code shouldBe "m"
      secondArg.canonicalName shouldBe "m"

      val List(firstArgOfLoweredCall: Identifier, secondArgOfLoweredCall: FieldIdentifier) =
        firstArg.argument.l: @unchecked
      firstArgOfLoweredCall.typeFullName shouldBe "mypkg.AClass$NamedCompanion"
      firstArgOfLoweredCall.refsTo.size shouldBe 0 // yes, 0. it's how the closed-source dataflow engine wants it atm
      secondArgOfLoweredCall.canonicalName shouldBe Constants.companionObjectMemberName
    }
  }
}
