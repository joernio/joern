package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._

class ExtensionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple extension function definitions" should {
    implicit val resolver = NoResolve

    lazy val cpg = code("""
        |package mypkg
        |
        |class Example {
        |    fun printBar() { println("class.bar") }
        |}
        |
        |fun Example.printBaz() { println("ext.baz") }
        |
        |fun main(args : Array<String>) {
        |  Example().printBaz()
        |}
        |""".stripMargin)

    "should contain a CALL node for the calls to the extension fns with the correct MFN set" in {
      val List(c) = cpg.call.code(".*printBaz.*").l
      c.methodFullName shouldBe "mypkg.Example.printBaz:void()"
    }

    "should contain a METHOD node for the extension fn with the correct MFN set" in {
      val List(m) = cpg.method.fullName(".*printBaz.*").l
      m.fullName shouldBe "mypkg.Example.printBaz:void()"
    }
  }

  "CPG for code with differently-scoped extension-fns on stdlib type" should {
    implicit val resolver = NoResolve

    // TODO: add test cases after the lowering is clear:
    //   --> right now we cannot differentiate between the fn definitions at different scopes
    lazy val cpg = code("""
        |package mypkg
        |
        |fun String.hash() = "HASH_PLACEHOLDER_1: $this"
        |
        |internal class AClass{
        |    private fun String.hash() = "HASH_PLACEHOLDER_2: $this"
        |    fun hashStr(str: String): String {
        |       val toHash = str.hash()
        |       return toHash
        |    }
        |}
        |
        |internal class BClass {
        |    private fun String.hash() = "HASH_PLACEHOLDER_3: $this"
        |    fun hashStr(str: String): String {
        |       val toHash = str.hash()
        |       return toHash
        |    }
        |}
        |
        |fun main() {
        |    val str = "MESSAGE"
        |    println(str.hash())
        |
        |    val c1 = AClass()
        |    println(c1.hashStr(str))
        |
        |    val c2 = BClass()
        |    println(c2.hashStr(str))
        |}
        |""".stripMargin)

    "should contain a CALL node with the correct props set for the call to the package-level defined extension fn" in {
      val List(c) = cpg.call.code("str.hash.*").where(_.method.fullName(".*main.*")).l
      c.methodFullName shouldBe "java.lang.String.hash:java.lang.String()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "java.lang.String()"
    }

    "should contain a CALL node with the correct props set for the call to the extension fn defined in `AClass`" in {
      val List(c) = cpg.typeDecl.fullName(".*AClass.*").method.fullName(".*hashStr.*").call.code("str.hash.*").l
      c.methodFullName shouldBe "java.lang.String.hash:java.lang.String()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "java.lang.String()"
    }

    "should contain a CALL node with the correct props set for the call to the extension fn defined in `BClass`" in {
      val List(c) = cpg.typeDecl.fullName(".*BClass.*").method.fullName(".*hashStr.*").call.code("str.hash.*").l
      c.methodFullName shouldBe "java.lang.String.hash:java.lang.String()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "java.lang.String()"
    }
  }
}
