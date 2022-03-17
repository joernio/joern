package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.semanticcpg.language._

class DefaultImportsTests extends AnyFreeSpec with Matchers {
  // It tests if we take into consideration default imports: https://kotlinlang.org/docs/packages.html#default-imports
  "cpg.typ.typeDeclFullName of constructed CPG should remain the same regardless if one uses default imports or explicitly uses FQNs" - {
    "for kotlin.collections.mapOf" in {
      val cpgExplicitFQNs = Kt2CpgTestContext.buildCpg("""
            |fun main(args: Array<String>) {
            |    val foo = kotlin.collections.mapOf("one" to 1)
            |}
            |""".stripMargin)

      val cpgDefaultImports = Kt2CpgTestContext.buildCpg("""
            |fun main(args: Array<String>) {
            |    val foo = mapOf("one" to 1)
            |}
            |""".stripMargin)

      val explicitRes       = cpgExplicitFQNs.typ.typeDeclFullName.toSetImmutable
      val defaultImportsRes = cpgDefaultImports.typ.typeDeclFullName.toSetImmutable
      explicitRes shouldBe defaultImportsRes
    }

    "for kotlin.comparisons.maxOf" in {
      val cpgExplicitFQNs = Kt2CpgTestContext.buildCpg("""
            |fun main(args: Array<String>) {
            |    val foo = kotlin.comparisons.maxOf(5, 100)
            |}
            |""".stripMargin)
      val cpgDefaultImports = Kt2CpgTestContext.buildCpg("""
            |fun main(args: Array<String>) {
            |    val foo = maxOf(5, 100)
            |}
            |""".stripMargin)

      val explicitRes       = cpgExplicitFQNs.typ.typeDeclFullName.toSetImmutable
      val defaultImportsRes = cpgDefaultImports.typ.typeDeclFullName.toSetImmutable
      explicitRes shouldBe defaultImportsRes
    }
  }
}
