package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import org.scalatest.Ignore

@Ignore
class LazyBlocksTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  // TODO: add test cases for lazy properties as well

  "CPG for code with simple lazy blocks" should {
    val cpg = code("""
        |package mypkg
        |
        |import java.nio.file.Files
        |
        |fun main() {
        |    val customDir = Files.createTempDirectory("custom").toFile()
        |    val foo = lazy { customDir }
        |    println(foo)
        |}
        |""".stripMargin)

    // TODO: add test cases
  }
}
