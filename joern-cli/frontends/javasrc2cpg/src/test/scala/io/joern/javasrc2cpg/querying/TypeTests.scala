package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class NewTypeTests extends JavaSrcCode2CpgFixture {
  "processing wildcard types should not crash (smoke test)" when {
    "the type is unbounded" in {
      val cpg = code("""
          |import java.net.URLClassLoader;
          |
          |class Foo {
          |  public void foo(URIClassLoader classLoader) {
          |    Class<?> cls = Class.forName("testName", true, classLoader);
          |    Object instance = cls.newInstance();
          |  }
          |}
          |""".stripMargin)

      cpg.identifier.name("instance").typeFullName.head shouldBe "java.lang.Object"
      cpg.call.name("newInstance").head.signature shouldBe "java.lang.Object()"
    }

    "there is a lower bound" in {
      val cpg = code("""
          |import java.net.URLClassLoader;
          |
          |class Foo {
          |  public void foo(URIClassLoader classLoader) {
          |    Class<? super Integer> cls = Class.forName("testName", true, classLoader);
          |    Object instance = cls.newInstance();
          |  }
          |}
          |""".stripMargin)

      cpg.identifier.name("instance").typeFullName.head shouldBe "java.lang.Object"
      cpg.call.name("newInstance").head.signature shouldBe "java.lang.Object()"
    }

    "there is an upper bound" in {
      val cpg = code("""
          |import java.net.URLClassLoader;
          |
          |class Foo {
          |  public void foo(URIClassLoader classLoader) {
          |    Class<? extends Number> cls = Class.forName("testName", true, classLoader);
          |    Object instance = cls.newInstance();
          |  }
          |}
          |""".stripMargin)

      cpg.identifier.name("instance").typeFullName.head shouldBe "java.lang.Object"
      cpg.call.name("newInstance").head.signature shouldBe "java.lang.Object()"
    }
  }

  "methods with varargs" should {
    val cpg = code("""
        |class Foo {
        |  public static String[] foo(boolean b, String... items) {
        |    return b ? items : new String[1];
        |  }
        |
        |  public void test(boolean b, String item1, String item2) {
        |    String[] items = foo(b, item1, item2);
        |  }
        |}
        |""".stripMargin)

    "use an array type to represent varargs in the method signature" in {
      cpg.method.name("foo").l match {
        case fooMethod :: Nil =>
          fooMethod.fullName shouldBe "Foo.foo:java.lang.String[](boolean,java.lang.String[])"
          fooMethod.signature shouldBe "java.lang.String[](boolean,java.lang.String[])"

        case result => fail(s"Expected single foo method but got $result")
      }
    }

    "create an array parameter node to represent varargs" in {
      def fooMethods = cpg.method.name("foo").l
      fooMethods.size shouldBe 1
      fooMethods.parameter.l match {
        case bParam :: itemsParam :: Nil =>
          bParam.typeFullName shouldBe "boolean"
          bParam.name shouldBe "b"
          bParam.code shouldBe "boolean b"

          itemsParam.typeFullName shouldBe "java.lang.String[]"
          itemsParam.name shouldBe "items"
          itemsParam.code shouldBe "String... items"

        case result => fail(s"Expected 2 parameters for foo method but got $result")
      }
    }

    "use an array type to represent varargs in the call signature" in {
      cpg.call.name("foo").l match {
        case fooCall :: Nil =>
          fooCall.methodFullName shouldBe "Foo.foo:java.lang.String[](boolean,java.lang.String[])"
          fooCall.signature shouldBe "java.lang.String[](boolean,java.lang.String[])"

        case result => fail(s"Expected single foo call but got $result")
      }
    }

    "use an arrayInitializer call node to represent varargs in the call AST" in {
      def call = cpg.call.name("foo")
      call.size shouldBe 1
      call.argument.l match {
        case List(_: Identifier, varargs: Call) =>
          varargs.methodFullName shouldBe Operators.arrayInitializer
          varargs.typeFullName shouldBe "java.lang.String[]"
          varargs.argument.l match {
            case List(item1: Identifier, item2: Identifier) =>
              item1.name shouldBe "item1"
              item1.typeFullName shouldBe "java.lang.String"

              item2.name shouldBe "item2"
              item2.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected array initializer args matching 2 items but got $result")
          }

        case result => fail(s"Expected identifier and arrayInitializer arguments but got $result")
      }
    }

    "lambda method implementing multi-abstract-method interface should be created correctly" in {
      val cpg = code("""
          |import java.util.ArrayList;
          |
          |public class Test {
          |    public static void main(String[] args) {
          |        ArrayList<Integer> xs = new ArrayList<Integer>();
          |        xs.sort((o1, o2) -> o1 - o2);
          |    }
          |}
          |""".stripMargin)
      cpg.method.nameExact("<lambda>0").fullName.l match {
        case List(fullName) => fullName shouldBe "Test.<lambda>0:int(java.lang.Object,java.lang.Object)"

        case res => fail(s"Expected fullName but got $res")
      }
    }
  }
}

class TypeTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
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
      |""".stripMargin)

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
    val List(x) = cpg.typ("Long").memberOfType.l
    x.name shouldBe "x"
  }

  "should allow traversing from return params TYPE to return param" in {
    val List(x) = cpg.typ("Integer").methodReturnOfType.l
    x.typeFullName shouldBe "java.lang.Integer"
  }

  "should allow traversing from params TYPE to param" in {
    val List(x) = cpg.typ("Object").parameterOfType.l
    x.name shouldBe "param"
  }

  "should allow traversing from local's TYPE to local" in {
    val List(x) = cpg.typ("Double").localOfType.l
    x.name shouldBe "y"
  }

  "should default to <unresolvedType> with a matching type node for unresolved types" in {
    val List(node) = cpg.identifier.name("UnknownType").l
    node.typeFullName shouldBe "ANY"
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

  "should use correct type for super calls in the constructor of foo.Foo" in {
    val List(call) = cpg.call.name(io.joern.x2cpg.Defines.ConstructorMethodName).l
    call.methodFullName shouldBe s"java.lang.Object.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
    call.typeFullName shouldBe "void"
    call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString

    call.argument(0).collect { case identifier: Identifier => identifier }.l match {
      case identifier :: Nil =>
        identifier.name shouldBe "this"
        identifier.typeFullName shouldBe "java.lang.Object"
        identifier.order shouldBe 1
        identifier.argumentIndex shouldBe 0

      case _ => fail("No receiver for super call found")
    }

  }

}
