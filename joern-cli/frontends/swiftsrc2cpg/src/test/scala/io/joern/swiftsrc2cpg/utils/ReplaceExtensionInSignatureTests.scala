package io.joern.swiftsrc2cpg.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ReplaceExtensionInSignatureTests extends AnyFunSuite with Matchers {

  private def callReplaceExtensionInSignature(fullName: String): String = {
    val provider = SwiftTypesProvider(config = null, parsedSwiftInvocations = Seq.empty)
    val m        = provider.getClass.getDeclaredMethod("replaceExtensionInSignature", classOf[String])
    m.setAccessible(true)
    m.invoke(provider, fullName).asInstanceOf[String]
  }

  test("replaceExtensionInSignature leaves strings without extension signature unchanged") {
    val in = "Foo.Bar.baz()"
    callReplaceExtensionInSignature(in) shouldBe in
  }

  test("replaceExtensionInSignature replaces a single '(extension in X)' occurrence") {
    val in  = "(extension in FooExt):Foo.bar() -> Swift.String"
    val out = "FooExt.Foo.bar() -> Swift.String"
    callReplaceExtensionInSignature(in) shouldBe out
  }

  test("replaceExtensionInSignature applies replacements repeatedly until no matches remain") {
    val in  = "(extension in A):A.f() -> (extension in B):B.g() -> Swift.Int"
    val out = "A.f() -> B.g() -> Swift.Int"
    callReplaceExtensionInSignature(in) shouldBe out
  }

  test("replaceExtensionInSignature strips duplicate name prefix after replacement") {
    val in  = "(extension in FooExt):FooExt.value : Swift.Int"
    val out = "FooExt.value : Swift.Int"
    callReplaceExtensionInSignature(in) shouldBe out
  }
}
