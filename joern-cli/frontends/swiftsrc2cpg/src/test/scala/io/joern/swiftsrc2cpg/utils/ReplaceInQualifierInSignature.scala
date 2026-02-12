package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReplaceInQualifierInSignature extends AnyWordSpec with Matchers {

  private val provider = SwiftTypesProvider(Config(), parsedSwiftInvocations = Nil)

  "SwiftTypesProvider.replaceInQualifierInSignature" should {

    "leave input unchanged if there is no '(TypeName in _ID...)' fragment" in {
      provider.replaceInQualifierInSignature("Foo.bar()") shouldBe "Foo.bar()"
      provider.replaceInQualifierInSignature("Foo.bar(in: 1)") shouldBe "Foo.bar(in: 1)"
    }

    "rewrite one '(TypeName in _ID...)' occurrence" in {
      val in       = "MyModule.(Type in _ID0123456789ABCDEF).member"
      val expected = "MyModule.Type.member"
      provider.replaceInQualifierInSignature(in) shouldBe expected
    }

    "rewrite multiple occurrences recursively" in {
      val in       = "A.(TypeA in _ID1).B.(TypeB in _ID2).C"
      val expected = "A.TypeA.B.TypeB.C"
      provider.replaceInQualifierInSignature(in) shouldBe expected
    }
  }

  "SwiftTypesProvider signature normalization" should {
    "work when both extension and in-rewrites are applied in sequence" in {
      val in       = "(extension in FooExt):Foo.(Type in _ID0123).bar()"
      val expected = "FooExt.Foo.Type.bar()"
      val out      = provider.replaceInQualifierInSignature(provider.replaceExtensionInSignature(in))
      out shouldBe expected
    }
  }
}
