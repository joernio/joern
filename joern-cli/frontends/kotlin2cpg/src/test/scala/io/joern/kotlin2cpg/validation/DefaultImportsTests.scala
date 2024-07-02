package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DefaultImportsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  // It tests if we take into consideration default imports: https://kotlinlang.org/docs/packages.html#default-imports
  "cpg.typ.typeDeclFullName of constructed CPG should remain the same regardless if one uses default imports or explicitly uses FQNs" should {
    "for kotlin.collections.mapOf" in {
      val cpgExplicitFQNs = code("""
            |fun main(args: Array<String>) {
            |    val foo = kotlin.collections.mapOf("one" to 1)
            |}
            |""".stripMargin)

      val cpgDefaultImports = code("""
            |fun main(args: Array<String>) {
            |    val foo = mapOf("one" to 1)
            |}
            |""".stripMargin)

      val explicitRes       = cpgExplicitFQNs.typ.typeDeclFullName.toSetImmutable
      val defaultImportsRes = cpgDefaultImports.typ.typeDeclFullName.toSetImmutable
      explicitRes shouldBe defaultImportsRes
    }

    "for kotlin.comparisons.maxOf" in {
      val cpgExplicitFQNs = code("""
            |fun main(args: Array<String>) {
            |    val foo = kotlin.comparisons.maxOf(5, 100)
            |}
            |""".stripMargin)
      val cpgDefaultImports = code("""
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
