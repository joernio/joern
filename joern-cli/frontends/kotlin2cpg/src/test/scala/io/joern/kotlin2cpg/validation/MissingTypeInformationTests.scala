package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

class MissingTypeInformationTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with CALL to Java stdlib fn with argument of unknown type" should {
    lazy val cpg = code("""
        |import no.such.package.CommandMaker
        |fun fetchDailyDaveArticle(maker: CommandMaker) {
        |   val cmd = maker.make("curl -s https://seclists.org/dailydave/2021/q4/0")
        |   Runtime.getRuntime().exec(cmd)
        |}
        |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("Runtime.getRuntime().exec(cmd)").l
      c.methodFullName shouldBe s"java.lang.Runtime.exec:${Defines.UnresolvedSignature}(1)"
    }
  }

  "CPG for code with CALL to Kotlin stdlib fn with argument of unknown type" should {
    lazy val cpg = code("""
      |import no.such.package.UrlStringFixer
      |fun bibleurlprint(fixer: UrlStringFixer) {
      |   val fixedUrl = fixer.fix("https://pocorgtfo.hacke.rs/pocorgtfo00.pdf")
      |   println(fixedUrl)
      |}
      |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("println(fixedUrl)").l
      c.methodFullName shouldBe s"kotlin.io.println:${Defines.UnresolvedSignature}(1)"
    }
  }
}
