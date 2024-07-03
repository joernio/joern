package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class PocTest extends PhpCode2CpgFixture {

  "The CPG generated for a very simple example" should {
    val cpg = code(
      """
        |<?php
        |function printHello($name) {
        |  echo "Hello, $name\n";
        |}
        |
        |printHello("Clarice");
        |""".stripMargin,
      fileName = "printhello.php"
    )

    "have the correct namespace set" in {
      cpg.namespaceBlock.fullName(".*printhello.php.*").l match {
        case namespaceBlock :: Nil =>
          namespaceBlock.name shouldBe "<global>"
        case result => fail(s"expected namespaceBlock found $result")
      }
    }

    "have a call node for the printHello call" in {
      cpg.call.nameExact("printHello").l match {
        case call :: Nil =>
          call.lineNumber shouldBe Some(7)

        case result => fail(s"Expected printHello call got $result")
      }
    }

    "have the correct method ast for the printHello method" in {
      cpg.method.internal.name("printHello").l match {
        case method :: Nil =>
          val List(param) = method.parameter.l
          param.name shouldBe "name"
          param.code shouldBe "$name"
          method.methodReturn.typeFullName shouldBe TypeConstants.Any

          val List(echoCall) = method.body.astChildren.collectAll[Call].l
          echoCall.name shouldBe "echo"
          echoCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          val encapsCall = echoCall.argument.l match {
            case List(encapsCall: Call) => encapsCall
            case result                 => fail(s"Expected encaps call but got $result")
          }

          encapsCall.argument.l match {
            case List(str1: Literal, identifier: Identifier, str2: Literal) =>
              str1.typeFullName shouldBe TypeConstants.String
              str1.code shouldBe "\"Hello, \""

              identifier.name shouldBe "name"

              str2.typeFullName shouldBe TypeConstants.String
              str2.code shouldBe "\"\\n\""

            case result => fail(s"Expected 3 part encaps call but got $result")
          }

        case result => fail(s"Expected printHello method but got $result")
      }
    }

  }
}
