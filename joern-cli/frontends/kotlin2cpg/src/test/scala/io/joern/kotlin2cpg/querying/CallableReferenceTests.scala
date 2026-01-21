package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef}
import io.shiftleft.semanticcpg.language.*

class CallableReferenceTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "resolved callable references as call argument should be handled correctly" in {
    val cpg = code("""
        |package com.test
        |
        |class Bar {
        |  fun bar(x: Int) {}
        |}
        |
        |class Foo {
        |  fun doNothing(c: (Int) -> Unit) {}
        |
        |  fun foo() {
        |    doNothing(Bar::bar)
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "com.test.Bar.bar:void(int)"
      methodRef.typeFullName shouldBe "com.test.Bar"
      methodRef.code shouldBe "Bar::bar"
    }
  }

  "callable references with unresolved signature should be handled correctly" in {
    val cpg = code("""
        |package com.test
        |
        |class Foo {
        |  fun doNothing(c: (Int) -> Unit) {}
        |  
        |  fun foo() {
        |    doNothing(Bar::bar)
        |  }
        |}
        |""".stripMargin)

    // When Bar class doesn't exist, the callable reference should still be created
    val methodRefs = cpg.methodRef.code("Bar::bar").l
    methodRefs.size shouldBe 1
    val methodRef = methodRefs.head

    methodRef.methodFullName shouldBe "<unresolvedNamespace>.bar:<unresolvedSignature>"
    methodRef.typeFullName shouldBe "ANY"
    methodRef.code shouldBe "Bar::bar"
  }

  "unresolved callable references should be handled correctly" in {
    val cpg = code("""
        |fun main() {
        |  someFunction(::unknownFunction)
        |}
        |""".stripMargin)

    val methodRefs = cpg.methodRef.code("::unknownFunction").l
    methodRefs.size shouldBe 1
    val methodRef = methodRefs.head

    methodRef.methodFullName shouldBe "<unresolvedNamespace>.unknownFunction:<unresolvedSignature>"
    methodRef.typeFullName shouldBe "ANY"
    methodRef.code shouldBe "::unknownFunction"
  }

  "resolved instance method refs should be handled correctly" in {
    val cpg = code("""
        |package com.test
        |
        |class Foo {
        |  fun doNothing(c: (Int) -> Unit) {}
        |
        |  fun func(x: Int) {}
        |
        |  fun foo() {
        |    val f = Foo()
        |    doNothing(f::func)
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "com.test.Foo.func:void(int)"
      methodRef.typeFullName shouldBe "com.test.Foo"
      methodRef.code shouldBe "f::func"
    }
  }

  "instance method refs with 'this' receiver should be handled correctly" in {
    val cpg = code("""
        |package com.test
        |
        |class Foo {
        |  fun doNothing(c: (Int) -> Unit) {}
        |
        |  fun func(x: Int) {}
        |
        |  fun foo() {
        |    doNothing(this::func)
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "com.test.Foo.func:void(int)"
      methodRef.typeFullName shouldBe "com.test.Foo"
      methodRef.code shouldBe "this::func"
    }
  }

  "callable references with collection receiver should be handled correctly" in {
    val cpg = code("""
        |fun main() {
        |  val numbers = listOf(1, 2, 3)
        |  val reference = numbers::forEach
        |}
        |""".stripMargin)

    val methodRefs = cpg.methodRef.code("numbers::forEach").l
    methodRefs.size shouldBe 1
    val methodRef = methodRefs.head

    // The exact type will depend on Kotlin stdlib resolution
    methodRef.methodFullName should include("forEach")
    methodRef.typeFullName should include("List")
    methodRef.code shouldBe "numbers::forEach"
  }

  "unbound callable references to top-level functions should be handled correctly" in {
    val cpg = code("""
        |fun isOdd(x: Int) = x % 2 != 0
        |
        |fun main() {
        |  val numbers = listOf(1, 2, 3)
        |  numbers.filter(::isOdd)
        |}
        |""".stripMargin)

    inside(cpg.call.name("filter").argument.l) { case List(receiver, methodRef: MethodRef) =>
      methodRef.methodFullName shouldBe "<unresolvedNamespace>.isOdd:boolean(int)"
      methodRef.typeFullName shouldBe "ANY"
      methodRef.code shouldBe "::isOdd"
    }
  }
}
