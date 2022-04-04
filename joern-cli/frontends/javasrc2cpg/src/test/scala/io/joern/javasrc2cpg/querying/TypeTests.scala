package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

class TypeTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      | package foo;
      |
      | class Foo {
      |   Long x;
      |
      |   Integer myFunc(Object param) {
      |     Double y;
      |     return 1;
      |   }
      |
      |   void foo() {
      |     UnknownType.run();
      |   }
      |
      |   public Foo() {
      |     super();
      |   }
      |
      |   static void bar(int[] xs) {}
      |
      |   static void baz(Foo[] fs) {}
      |
      |   static void bak(Foo... fs) {}
      | }
      |
      | class Bar extends A<B<C>> {
      |   public D<E<F>> foo() {
      |
      |   }
      |   @Override
      |   public Class<?> getObjectType() {
      |     return Bar.class;
      |   }
      | }
      |
      |""".stripMargin

  "should create TYPE node with correct fields for class" in {
    val List(x) = cpg.typ.name("Foo").l
    x.name shouldBe "Foo"
    x.fullName shouldBe "foo.Foo"
    x.typeDeclFullName shouldBe "foo.Foo"
  }

  "should create TYPE node with correct fields for class member" in {
    val List(x) = cpg.typ.name("Long").l
    x.name shouldBe "Long"
    x.fullName shouldBe "java.lang.Long"
    x.typeDeclFullName shouldBe "java.lang.Long"
  }

  "should create TYPE node with correct fields for return type" in {
    val List(x) = cpg.typ.name("Integer").l
    x.name shouldBe "Integer"
    x.fullName shouldBe "java.lang.Integer"
    x.typeDeclFullName shouldBe "java.lang.Integer"
  }

  "should create TYPE node with correct fields for parameter type" in {
    val List(x) = cpg.typ.name("Object").l
    x.name shouldBe "Object"
    x.fullName shouldBe "java.lang.Object"
    x.typeDeclFullName shouldBe "java.lang.Object"
  }

  "should create TYPE node with correct fields for local type" in {
    val List(x) = cpg.typ.name("Double").l
    x.name shouldBe "Double"
    x.fullName shouldBe "java.lang.Double"
    x.typeDeclFullName shouldBe "java.lang.Double"
  }

  "should allow traversing from member's TYPE to member" in {
    val List(x) = cpg.typ("java.lang.Long").memberOfType.l
    x.name shouldBe "x"
  }

  "should allow traversing from return params TYPE to return param" in {
    val List(x) = cpg.typ("java.lang.Integer").methodReturnOfType.l
    x.typeFullName shouldBe "java.lang.Integer"
  }

  "should allow traversing from params TYPE to param" in {
    val List(x) = cpg.typ("java.lang.Object").parameterOfType.l
    x.name shouldBe "param"
  }

  "should allow traversing from local's TYPE to local" in {
    val List(x) = cpg.typ("java.lang.Double").localOfType.l
    x.name shouldBe "y"
  }

  "should default to ANY with a matching type node for unresolved types" in {
    val List(x)    = cpg.typ("ANY").l
    val List(node) = cpg.identifier.name("UnknownType").l
    node.typeFullName shouldBe "ANY"
    node.typ.headOption shouldBe Some(x)
  }

  "should handle primitive type arrays" in {
    cpg.method.name("bar").parameter.name("xs").headOption match {
      case Some(param) =>
        param.typeFullName shouldBe "int[]"
        param.typ.name shouldBe "int[]"
        param.typ.fullName shouldBe "int[]"

      case res => fail(s"Expected single param xs but got $res")
    }
  }

  "should handle reference type arrays" in {
    cpg.method.name("baz").parameter.name("fs").headOption match {
      case Some(param) =>
        param.typeFullName shouldBe "foo.Foo[]"
        param.typ.name shouldBe "Foo[]"
        param.typ.fullName shouldBe "foo.Foo[]"

      case res => fail(s"Expected array parameter fs but got $res")
    }
  }

  "should use array type for varargs" in {
    cpg.method.name("bak").parameter.name("fs").headOption match {
      case Some(param) =>
        param.typeFullName shouldBe "foo.Foo[]"
        param.typ.name shouldBe "Foo[]"
        param.typ.fullName shouldBe "foo.Foo[]"

      case res => fail(s"Expected array parameter fs but got $res")
    }
  }

  "should use correct type for super calls" in {
    val List(call) = cpg.call.name("<init>").l
    call.methodFullName shouldBe "java.lang.Object.<init>:void()"
    call.typeFullName shouldBe "void"
    call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString

    call.receiver.collect { case identifier: Identifier => identifier }.l match {
      case identifier :: Nil =>
        identifier.name shouldBe "this"
        identifier.typeFullName shouldBe "java.lang.Object"
        identifier.order shouldBe 0
        identifier.argumentIndex shouldBe 0

      case _ => fail("No receiver for super call found")
    }

  }

}
