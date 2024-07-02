package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class ResolutionErrorsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with QE of receiver for which the type cannot be inferred" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |//    val foo = listOf(1, 2, 3) // purposely commented-out for the test to make sense
        |    val bar = foo.flatMap { x ->
        |        listOf(x, x + 1)
        |    }
        |    println(bar) // prints `[1, 2, 2, 3, 3, 4]`
        |}
        |""".stripMargin)

    "should contain a CALL node with an MFN starting with a placeholder type" in {
      val List(c) = cpg.call.slice(1, 2).l
      c.methodFullName shouldBe Defines.UnresolvedNamespace + ".flatMap:ANY(ANY)"
    }
  }

  "CPG for code with simple stdlib fns" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.collections.HashMap
        |
        |fun bar(): HashMap<String,String> {
        |    val x = HashMap<String, String>()
        |    return x
        |}
        |
        |fun foo(): Boolean {
        |    val c = bar()
        |    return false
        |}
        |""".stripMargin)

    // TODO: remove this test case as soon as there's a custom type descriptor renderer
    // this silly case is only necessary because the Kotlin compiler API writers decided,
    // for whatever reason, to not provide any way of rendering specific types without these
    // substrings in them
    "should not contain METHOD nodes with comments the substring `/*` them" in {
      val mfns = cpg.method.fullName.l
      mfns.filter { fn =>
        fn.contains("/*")
      } shouldBe Seq()
    }
  }

  "CPG for code with stdlib mutable list of items of class without type info available" should {
    val cpg = code("""
        |package mypkg
        |
        |import com.intellij.ide.util.projectWizard.importSources.DetectedProjectRoot
        |
        |fun foo(x: DetectedProjectRoot): Boolean {
        |    val list = mutableListOf<DetectedProjectRoot>()
        |    list.add(1, x)
        |    println(list)
        |}
        |""".stripMargin)

    "should not contain METHOD nodes with comments the substring `ERROR` in them" in {
      val mfns = cpg.method.fullName.l
      mfns.filter { fn =>
        fn.contains("ERROR") || fn.contains("???")
      } shouldBe Seq()
    }

    "should contain a CALL node with a placeholder MFN in it" in {
      val List(c) = cpg.call.code("mutableListOf.*").l
      c.methodFullName shouldBe "kotlin.collections.mutableListOf:java.util.List()"
    }
  }

  "CPG for code with QE expression without type info" should {
    val cpg = code("""
        |package mypkg
        |
        |object AClass {
        |    fun getList(): List<Int?> {
        |        return listOf(1, 2, 3, 4, 5, null)
        |    }
        |}
        |
        |fun main() {
        |    val foo =
        |            AClass.getList()
        |                    .mapNotNull { x -> x }
        |                    .filter { x -> x < 4 }
        |    println(foo) // prints `[1, 2, 3]`
        |
        |    val bar = NoSuchClass.getList()
        |                    .mapNotNull { x -> x }
        |                    .filter { x -> x < 4 }
        |    println(bar)
        |
        |}
        |""".stripMargin)

    "should contain a CALL node with the correct MFN set when type info is available" in {
      val List(c) = cpg.call.methodFullName(Operators.assignment).where(_.argument(1).code("foo")).argument(2).isCall.l
      c.methodFullName shouldBe "java.lang.Iterable.filter:java.util.List(kotlin.Function1)"
    }

    "should contain a CALL node with the correct MFN set when type info is not available" in {
      val List(c) = cpg.call.methodFullName(Operators.assignment).where(_.argument(1).code("bar")).argument(2).isCall.l
      c.methodFullName shouldBe Defines.UnresolvedNamespace + ".filter:ANY(ANY)"
    }
  }

  "CPG for code with extension fn defined on unresolvable type " should {
    val cpg = code("""
        |package mypkg
        |
        |import com.intellij.openapi.editor.*
        |
        |// `Editor` is the unresolvable type
        |fun Editor.getFileSize(countNewLines: Boolean = false): Int {
        |  val len = document.textLength
        |  val doc = document.charsSequence
        |  return if (countNewLines || len == 0 || doc[len - 1] != '\n') len else len - 1
        |}
        |
        |""".stripMargin)

    "should contain a METHOD node with a MFN property starting with `kotlin.Any`" in {
      val List(m) = cpg.method.fullName(".*getFileSize.*").l
      m.fullName shouldBe s"${Defines.UnresolvedNamespace}.getFileSize:int(boolean)"
    }
  }

  "CPG for code with extension fn defined on resolvable type with unresolvable subtypes" should {
    val cpg = code("""
        |package mypkg
        |
        |import com.intellij.openapi.editor.*
        |
        |// `Editor` and `IntArrayList` are unresolvable types
        |fun MutableMap<Editor, IntArrayList>.clone(): MutableMap<Editor, IntArrayList> {
        |  val clone = HashMap<Editor, IntArrayList>(size)
        |  for ((editor, offsets) in this) {
        |    clone[editor] = offsets.clone()
        |  }
        |  return clone
        |}
        |""".stripMargin)

    "should contain a METHOD node with a MFN property that replaced the unresolvable types with `kotlin.Any`" in {
      val List(m) = cpg.method.fullName(".*clone.*").take(1).l
      m.fullName shouldBe "java.util.Map.clone:java.util.Map()"
    }
  }

  "CPG for code with `containsKey` call on collection of elements without corresponding imported class" should {
    val cpg = code("""
        |package mypkg
        |
        |import io.no.SuchPackage
        |
        |fun render(results: Map<SuchPackage, IntList>) {
        |  var foo = mutableMapOf<SuchPackage, Array<Int>>()
        |  for (aKey in foo.keys.toList()) {
        |    if (!results.containsKey(aKey))
        |      println("DO!")
        |  }
        |}
        |
        |""".stripMargin)

    "should not contain any MFNs with `ERROR` in them" in {
      cpg.method.fullName(".*ERROR.*").fullName.l shouldBe List()
    }

    "should contain a METHOD node for `containsKey` with `java.lang.Object` in the place of the failed resolution" in {
      val List(m) = cpg.method.fullName(".*containsKey.*").l
      m.fullName shouldBe "kotlin.collections.Map.containsKey:boolean(java.lang.Object)"
    }
  }
}
