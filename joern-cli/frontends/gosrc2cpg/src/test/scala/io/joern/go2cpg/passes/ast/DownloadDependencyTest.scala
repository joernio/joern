package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.Config
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class DownloadDependencyTest extends GoCodeToCpgSuite {
  "Simple use case of thrid party dependency download use case" ignore {
    val config = Config().withFetchDependencies(true)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |require (
        |	github.com/google/uuid v1.3.1 // indirect
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
        |package main
        |import "github.com/google/uuid"
        |func main()  {
        |  var uud = uuid.NewString()
        |}
        |""".stripMargin)
      .withConfig(config)

    "Check CALL Node" in {
      val List(x) = cpg.call("NewString").l
      x.typeFullName shouldBe "string"
    }
  }

  "unresolved dependency tests one" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |require (
        |	joern.io/sampletwo v1.3.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import "joern.io/sampletwo"
          |func main()  {
          |  var a = sampletwo.Person{Name:"Pandurang"}
          |  var b = a.Name
          |  var c = a.FullName()
          |  var d = a.Process().FullName()
          |  var e = a.Process().SomeField
          |}
          |""".stripMargin)

    "Be correct for CALL Node typeFullNames" in {
      val List(a, b, c, d, e, f, g) = cpg.call.nameNot(Operators.assignment).l
      a.typeFullName shouldBe "joern.io/sampletwo.Person"
      b.typeFullName shouldBe "joern.io/sampletwo.Person.Name.<FieldAccess>.<unknown>"
      c.typeFullName shouldBe "joern.io/sampletwo.Person.FullName.<ReturnType>.<unknown>"
      d.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>.FullName.<ReturnType>.<unknown>"
      e.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>"
      f.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>.SomeField.<FieldAccess>.<unknown>"
    }
  }

  "unresolved dependency tests two" should {
    val cpg = code("""
        |package main
        |import (
        |    "github.com/rs/zerolog"
        |    "github.com/rs/zerolog/log"
        |)
        |func main() {
        |    zerolog.SetGlobalLevel(zerolog.InfoLevel)
        |    log.Error().Msg("Error message")
        |    log.Warn().Msg("Warning message")
        |}
        |""".stripMargin)

    "Be correct for CALL Node typeFullNames" in {
      val List(a, b, c, d, e) = cpg.call.nameNot(Operators.fieldAccess).l
      a.typeFullName shouldBe "github.com/rs/zerolog.SetGlobalLevel.<ReturnType>.<unknown>"
      b.typeFullName shouldBe "github.com/rs/zerolog/log.Error.<ReturnType>.<unknown>.Msg.<ReturnType>.<unknown>"
      c.typeFullName shouldBe "github.com/rs/zerolog/log.Error.<ReturnType>.<unknown>"
      d.typeFullName shouldBe "github.com/rs/zerolog/log.Warn.<ReturnType>.<unknown>.Msg.<ReturnType>.<unknown>"
      e.typeFullName shouldBe "github.com/rs/zerolog/log.Warn.<ReturnType>.<unknown>"
    }
  }
}
