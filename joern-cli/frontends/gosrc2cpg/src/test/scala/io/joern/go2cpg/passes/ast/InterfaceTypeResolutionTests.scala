package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import java.io.File

class InterfaceTypeResolutionTests extends GoCodeToCpgSuite {

  "Interface method metadata should be recorded" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |type Speaker interface {
        |  Speak() string
        |}
        |""".stripMargin,
      Seq("lib", "iface.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib"
        |func greet(s lib.Speaker) string {
        |  return s.Speak()
        |}
        |""".stripMargin,
      "main.go"
    )

    "resolve interface method call return type" in {
      val List(speakCall) = cpg.call("Speak").l
      speakCall.typeFullName shouldBe "string"
    }
  }
}
