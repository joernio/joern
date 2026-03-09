package io.joern.swiftsrc2cpg.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ReplaceExtensionInSignatureTests extends AnyFunSuite with Matchers {

  private val provider = SwiftTypesProvider(config = null, parsedSwiftInvocations = Seq.empty)

  test("replaceExtensionInSignature leaves strings without extension signature unchanged") {
    val in = "Foo.Bar.baz(param: Swift.Int) -> Swift.String"
    provider.replaceExtensionInSignature(in) shouldBe in
  }

  test("replaceExtensionInSignature replaces a single '(extension in X)' occurrence") {
    val in  = "Foo.bar(transform:(extension in A):A.a() ->Swift.String) -> Swift.String"
    val out = "Foo.bar(transform:A.a() ->Swift.String) -> Swift.String"
    provider.replaceExtensionInSignature(in) shouldBe out
  }

  test("replaceExtensionInSignature applies replacements repeatedly until no matches remain") {
    val in  = "A.f(t:(extension in A):A.a() ->Swift.String, c:(extension in B):B.g() ->Swift.Int) -> Swift.Int"
    val out = "A.f(t:A.a() ->Swift.String, c:B.g() ->Swift.Int) -> Swift.Int"
    provider.replaceExtensionInSignature(in) shouldBe out
  }

  test("replaceExtensionInSignature strips duplicate name prefix after replacement on type fullNames") {
    val in  = "(extension in FooExt):FooExt.value : Swift.Int"
    val out = "FooExt.value : Swift.Int"
    provider.replaceExtensionInSignature(in) shouldBe out
  }

  test("replaceExtensionInSignature applies replacements repeatedly but leaves the extension tag in call fullNames") {
    val in  = "Foo.Bar<extension>.apply:(_:Swift.String,transform:(extension in A):A.a() ->Swift.String)->Swift.String"
    val out = "Foo.Bar<extension>.apply:(_:Swift.String,transform:A.a() ->Swift.String)->Swift.String"
    provider.replaceExtensionInSignature(in) shouldBe out
  }
}
