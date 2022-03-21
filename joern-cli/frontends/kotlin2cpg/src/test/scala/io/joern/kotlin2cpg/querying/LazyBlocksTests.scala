package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.jIteratortoTraversal

class LazyBlocksTests extends AnyFreeSpec with Matchers {

  // TODO: add test cases for lazy properties as well

  "CPG for code with simple lazy blocks" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
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
