package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class GenericsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with class inheriting from generic interface" should {
    val cpg = code("""
    |interface Transformer<T, R> {
    | fun transform(p: T): R
    |}
    |
    |class TransformerImpl: Transformer<Int, Boolean> {
    | override fun transform(p: Int): Boolean{
    |   return p == 42
    | }
    |}
    """)

    val typedecls = cpg.typeDecl.name("TransformerImpl").l
    typedecls.size shouldBe 1
    val typedecl = typedecls.head

    "transform should contain two bindings" in {
      val bindings = typedecl.methodBinding.name("transform").l
      bindings.size shouldBe 2
    }

    "the transform method bindings should have the correct properties" in {
      val bindings = typedecl.methodBinding.name("transform").l.sortBy(_.signature)

      val List(full, generic) = bindings

      generic.signature shouldBe "java.lang.Object(java.lang.Object)"
      generic.methodFullName shouldBe "TransformerImpl.transform:boolean(int)"

      full.signature shouldBe "boolean(int)"
      full.methodFullName shouldBe "TransformerImpl.transform:boolean(int)"
    }

  }

  "CPG for code with class inheriting from generic interface with upper bound" should {
    val cpg = code("""
    |open class TestClass
    |
    |interface Transformer<T: TestClass, R> {
    | fun transform(p: T): R
    |}
    |
    |class TestClassImpl: TestClass
    |
    |class TransformerImpl: Transformer<TestClassImpl, Boolean> {
    | override fun transform(p: TestClassImpl): Boolean{
    |   return p is TestClass
    | }
    |}
    """)

    val typedecls = cpg.typeDecl.name("TransformerImpl").l
    typedecls.size shouldBe 1
    val typedecl = typedecls.head

    "transform should contain two bindings" in {
      val bindings = typedecl.methodBinding.name("transform").l
      bindings.size shouldBe 2
    }

    "the transform method bindings should have the correct properties" in {
      val bindings = typedecl.methodBinding.name("transform").l.sortBy(_.signature)

      val List(full, generic) = bindings

      generic.signature shouldBe "java.lang.Object(TestClass)"
      generic.methodFullName shouldBe "TransformerImpl.transform:boolean(TestClassImpl)"

      full.signature shouldBe "boolean(TestClassImpl)"
      full.methodFullName shouldBe "TransformerImpl.transform:boolean(TestClassImpl)"
    }
  }

  "CPG for code with simple user-defined fn using generics" should {
    val cpg = code("""
        |
        |package mypkg
        |
        |open class AClass(msg: String)
        |open class BClass(msg: String) : AClass(msg)
        |open class CClass(msg: String) : AClass(msg)
        |
        |fun <T> getAnInstance(of: String): T? {
        |    if (of == "b") {
        |        return BClass(of) as T
        |    } else if (of == "c") {
        |        return CClass(of) as T
        |    } else {
        |        return AClass(of) as T
        |    }
        |}
        |
        |fun main() {
        |    val p1 = getAnInstance<BClass>("b")
        |    println(p1)
        |    val p2 = getAnInstance<CClass>("c")
        |    println(p2)
        |}
        |
        |""".stripMargin)

    "should contain IDENTIFIER nodes with the correct TYPE_FULL_NAME props set" in {
      cpg.identifier.codeExact("p1").typeFullName.toSet shouldBe Set("mypkg.BClass")
      cpg.identifier.codeExact("p2").typeFullName.toSet shouldBe Set("mypkg.CClass")
    }

    "should contain CALL nodes with the correct METHOD_FULL_NAME props set" in {
      val List(c1) = cpg.call.code("getAnInstance.*").take(1).l
      c1.methodFullName shouldBe "mypkg.getAnInstance:java.lang.Object(java.lang.String)"
      val List(c2) = cpg.call.code("getAnInstance.*").slice(1, 2).l
      c2.methodFullName shouldBe "mypkg.getAnInstance:java.lang.Object(java.lang.String)"
    }

    "should contain a METHOD node for the fn using generics with the correct METHOD_FULL_NAME set" in {
      val List(m) = cpg.method.fullName(".*getAnInstance.*").take(1).l
      m.fullName shouldBe "mypkg.getAnInstance:java.lang.Object(java.lang.String)"
    }
  }

  "CPG for code with simple user-defined fn using generics and a type parameter subclassed from user-defined fn" should {
    val cpg = code("""
        |
        |package mypkg
        |
        |open class AClass(msg: String)
        |open class BClass(msg: String) : AClass(msg)
        |open class CClass(msg: String) : AClass(msg)
        |
        |fun <T : AClass> getAnInstance(of: String): T? {
        |    if (of == "b") {
        |        return BClass(of) as T
        |    } else if (of == "c") {
        |        return CClass(of) as T
        |    } else {
        |        return AClass(of) as T
        |    }
        |}
        |
        |fun main() {
        |    val p1 = getAnInstance<BClass>("b")
        |    println(p1)
        |    val p2 = getAnInstance<CClass>("c")
        |    println(p2)
        |}
        |
        |""".stripMargin)

    "should contain IDENTIFIER nodes with the correct TYPE_FULL_NAME props set" in {
      cpg.identifier.codeExact("p1").typeFullName.toSet shouldBe Set("mypkg.BClass")
      cpg.identifier.codeExact("p2").typeFullName.toSet shouldBe Set("mypkg.CClass")
    }

    "should contain CALL nodes with the correct METHOD_FULL_NAME props set" in {
      val List(c1) = cpg.call.code("getAnInstance.*").take(1).l
      c1.methodFullName shouldBe "mypkg.getAnInstance:mypkg.AClass(java.lang.String)"
      val List(c2) = cpg.call.code("getAnInstance.*").slice(1, 2).l
      c2.methodFullName shouldBe "mypkg.getAnInstance:mypkg.AClass(java.lang.String)"
    }

    "should contain a METHOD node for the fn using generics with the correct METHOD_FULL_NAME set" in {
      val List(m) = cpg.method.fullName(".*getAnInstance.*").take(1).l
      m.fullName shouldBe "mypkg.getAnInstance:mypkg.AClass(java.lang.String)"
    }
  }
}
