package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class GenericSignatureTests extends JavaSrcCode2CpgFixture {

  "a simple example with primitive types" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  char charMember;
        |
        |  public void test(boolean b) {
        |    int x;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for locals" in {
      cpg.local.genericSignature.l shouldBe List("I")
    }

    "have the correct generic signature for a void method with a boolean arg" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(Z)V")
    }

    "have the correct generic signature for the parameter" in {
      cpg.member.name("charMember").genericSignature.l shouldBe List("C")
    }

    "have the correct generic signature for the type decl implicitly extending java.lang.Object" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Ljava/lang/Object;")
    }
  }

  "a method with parameters and a non-void return type" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public String test(Test t, Integer i) {
        |    return null;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(Ltest/Test;Ljava/lang/Integer;)Ljava/lang/String;")
    }
  }

  "a method with an unresolved return type" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public Foo test(Test t) {
        |    return null;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List(
        "(Ltest/Test;)L__unresolved_namespace_placeholder__/Foo;"
      )
    }
  }

  "a method with an unresolved parameter" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public void test(Foo f) {
        |    return null;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(L__unresolved_namespace_placeholder__/Foo;)")
    }
  }

  "a class extending another class" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {}
        |""".stripMargin).moreCode("""
        |package test;
        |
        |class Test extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a class implementing an interface" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin).moreCode("""
        |package test;
        |
        |class Test implements Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a class extending another class and implementing an interface" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {}
        |""".stripMargin)
      .moreCode("""
        |package bar;
        |
        |interface Bar {}
        |""".stripMargin)
      .moreCode("""
        |package test;
        |
        |class Test extends Foo implements Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;Lfoo/Bar;")
    }
  }

  "a class implementing multiple interfaces" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin)
      .moreCode("""
        |package bar;
        |
        |interface Bar {}
        |""".stripMargin)
      .moreCode("""
        |package test;
        |
        |class Test implements Foo, Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;Lfoo/Bar;")
    }
  }

  "an interface not extending another interface" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Foo").genericSignature.l shouldBe List("Ljava/lang/Object;")
    }
  }

  "an interface extending another interface" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin).moreCode("""
        |package bar;
        |
        |interface Bar extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Bar").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "an interface extending multiple interfaces" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin)
      .moreCode("""
        |package bar;
        |
        |interface Bar {}
        |""".stripMargin)
      .moreCode("""
        |package test;
        |
        |interface Test extends Foo, Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;Lbar/Bar;")
    }
  }

  "a class extending an unresolved class" should {
    val cpg = code("""
        |package test;
        |
        |class Test extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("L__unresolved_namespace_placeholder__/Foo;")
    }
  }

  "a class implementing an unresolved interface" should {
    val cpg = code("""
        |package test;
        |
        |class Test implements Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("L__unresolved_namespace_placeholder__/Foo;")
    }
  }

  "a resolved lambda method" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.function.Consumer;
        |
        |class Test {
        |  public Consumer<String> test() {
        |    return s -> System.out.println(s);
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the lambda method" in {
      // TODO should this be the erased type?
      cpg.method.name(".*lambda.*").genericSignature.l shouldBe List("(Ljava/lang/String;)V")
    }

    "have the correct generic signature for the lambda decl" in {
      // TODO should this include type variables?
      cpg.typeDecl.name(".*lambda.*").genericSignature.l shouldBe List("Ljava/util/function/Consumer;")
    }
  }

  "an unresolved lambda method" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public Consumer<String> test() {
        |    return s -> System.out.println(s);
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the lambda method" in {
      ???
    }

    "have the correct generic signature for the lambda decl" in {
      ???
    }
  }

  "a nested class" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  class Nested {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Nested").genericSignature.l shouldBe List("Ljava/lang/Object;")
    }
  }

  "a local class" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public void test() {
        |    class Local {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Nested").genericSignature.l shouldBe List("Ljava/lang/Object;")
    }
  }

  "an anonymous class extending a resolved class" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {}
        |""".stripMargin).moreCode("""
        |package test;
        |
        |import foo.Foo;
        |
        |class Test {
        |  public void test() {
        |    Foo f = new Foo() {};
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "an anonymous class extending an unresolved class" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public void test() {
        |    Foo f = new Foo() {};
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("L__unresolved_namespace_placeholder__/Foo;")
    }
  }

  "an anonymous class extending an unresolved class which can be resolved from imports" should {
    val cpg = code("""
        |package test;
        |
        |import foo.Foo;
        |
        |class Test {
        |  public void test() {
        |    Foo f = new Foo() {};
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a local with an array type" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public void test() {
        |    String[] items;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("items").genericSignature.l shouldBe List("[Ljava/lang/String")
    }
  }

  "a generic local with a single type argument" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public void test() {
        |    List<String> list;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("list").genericSignature.l shouldBe List("Ljava/util/List<Ljava/lang/String;>;")
    }
  }

  "a generic local with a single wildcard type argument" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public void test() {
        |    List<?> list;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("list").genericSignature.l shouldBe List("Ljava/util/List<*>;")
    }
  }

  "a generic local with a single wildcard type argument with an upper bound" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public void test() {
        |    List<? extends String> list;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("list").genericSignature.l shouldBe List("Ljava/util/List<+Ljava/lang/String;>;")
    }
  }

  "a generic local with a single wildcard type argument with a lower bound" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public void test() {
        |    List<? super String> list;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("list").genericSignature.l shouldBe List("Ljava/util/List<-Ljava/lang/String;>;")
    }
  }

  "a generic local with multiple type arguments" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.Map;
        |
        |class Test {
        |  public void test() {
        |    Map<String, Integer> map;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("map").genericSignature.l shouldBe List("Ljava/util/Map<Ljava/lang/String;Ljava/lang/Integer;>;")
    }
  }

  "a generic local with nested type arguments" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |import java.util.Map;
        |
        |class Test {
        |  public void test() {
        |    Map<String, List<String>> map;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("map").genericSignature.l shouldBe List(
        "Ljava/util/Map<Ljava/lang/String;Ljava/util/List<Ljava/lang/String;>;>;"
      )
    }
  }

  "a generic local with a type variable type from the method signature" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public <T> void test() {
        |    T t;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("t").genericSignature.l shouldBe List("TT;")
    }
  }

  "a generic local with a nested type variable type from the method signature" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public <S> void test() {
        |    List<S> list;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("list").genericSignature.l shouldBe List("Ljava/util/List<TS;>;")
    }
  }

  "a generic local with a type variable type from the class signature" should {
    val cpg = code("""
        |import java.util.List;
        |
        |public class Main <T> {
        |    public void main(String[] args) {
        |        T t;
        |    }
        |}
        |
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("t").genericSignature.l shouldBe List("TT;")
    }
  }

  "a generic local with a type variable type as a bound from the class signature" should {
    val cpg = code("""
        |import java.util.List;
        |
        |public class Main <T> {
        |    public void main(String[] args) {
        |        List<? extends T> t;
        |    }
        |}
        |
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("t").genericSignature.l shouldBe List("Ljava/util/List<+TT;>;")
    }
  }

  "a method with a generic return type and generic parameters" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public <S, T extends List> S test(T t) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("<S:Ljava/lang/Object;T::Ljava/util/List;>(TT;)TS;")
    }
  }

  "a generic member" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  public List<String> list;
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.member.name("list").genericSignature.l shouldBe List("Ljava/util/List<Ljava/lang/String;>;")
    }
  }

  "an enum typeDecl" should {
    val cpg = code("""
        |package test;
        |
        |enum Test {
        |  TEST
        |}
        |""".stripMargin)

    "have the correct generic signature for the type decl" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Ljava/lang/Enum;")
    }

    "have the correct generic signature for the enum constant" in {
      cpg.member.name("TEST").genericSignature.l shouldBe List("Ltest/Test;")
    }
  }

  "a record typeDecl" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |record Test<T>(String value, List<T> list) {}
        |""".stripMargin)

    "have the correct generic signature for the record" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("<T:Ljava/lang/Object;>Ljava/lang/Record;")
    }

    "have the correct generic signature for the record parameter fields" in {
      cpg.member.name("value").genericSignature.l shouldBe List("Ljava/lang/String;")
      cpg.member.name("list").genericSignature.l shouldBe List("Ljava/util/List<TT;>;")
    }

    "have the correct generic signature for the default constructor" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(Ljava/lang/String;Ljava/util/List<TT;>;)V")
    }

    "have the correct generic signature for the record paramater accessors" in {
      cpg.method.name("value").genericSignature.l shouldBe List("()Ljava/lang/String;")
      cpg.method.name("list").genericSignature.l shouldBe List("()Ljava/util/List<TT;>;")
    }
  }

  "a type decl extending a generic type" should {
    val cpg = code("""
        |package bar;
        |
        |class Bar <T> {}
        |""".stripMargin).moreCode("""
        |package test;
        |
        |class Test extends Bar<String> {}
        |""".stripMargin)

    "have the correct generic signature for the generic class" in {
      cpg.typeDecl.name("Bar").genericSignature.l shouldBe List("<T:Ljava/lang/Object;>Ljava/lang/Object;")
    }

    "have the correct generic signature for the inheriting class" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lbar/Bar<Ljava/lang/String;>;")
    }
  }

  "teh lowering for a native foreach loop" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  void test(String[] items) {
        |    for (String item : items) {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the synthetic iterator local" in {
      cpg.local.nameExact("$idx0").genericSignature.l shouldBe List("I")
    }

    "have the correct generic signature for the variable local" in {
      cpg.local.name("item").genericSignature.l shouldBe List("Ljava/lang/String;")
    }
  }

  "the lowering for an iterator foreach loop" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.List;
        |
        |class Test {
        |  void test(List<String> items) {
        |    for (String item : items) {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the synthetic iterator local" in {
      cpg.local.nameExact("$iterLocal0").genericSignature.l shouldBe List("Ljava/util/Iterator;")
    }

    "have the correct generic signature for the variable local" in {
      cpg.local.name("item").genericSignature.l shouldBe List("Ljava/lang/String;")
    }
  }

  "the synthetic tmp local in the block representation of a constructor invocation" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  void test() {
        |    System.out.println(new String());
        |  }
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.nameExact("$tmp0").genericSignature.l shouldBe List("Ljava/lang/String;")
    }
  }

  "a captured local in a lambda" should {
    val cpg = code("""
        |package test;
        |
        |import java.util.function.Consumer;
        |
        |class Test {
        |  public Consumer<String> test(Integer captured) {
        |    return s -> System.out.println(captured);
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("captured").genericSignature.l shouldBe List("I")
    }
  }

  "a pattern initializer requiring a tmp local" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  public Object foo() { return null; }
        |
        |  public void test() {
        |    if (foo() instanceof String s) {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the tmp local" in {
      cpg.local.nameExact("$obj0").genericSignature.l shouldBe List("Ljava/lang/Object;")
    }

    "have the correct generic signature for the pattern variable local" in {
      cpg.local.name("s").genericSignature.l shouldBe List("Ljava/lang/String;")
    }
  }

  "a local class with captures" should {
    val cpg = code("""
        |class Test {
        |  String mainField;
        |
        |  public void test(Integer testParam) {
        |    class Foo {
        |      void foo() {
        |        System.out.println(mainField + testParam);
        |      }
        |    }
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the outerClass member" in {
      cpg.member.name("outerClass").genericSignature.l shouldBe List("LTest;")
    }

    "have the correct generic signature for a captured parameter member" in {
      cpg.member.name("testParam").genericSignature.l shouldBe List("Ljava/lang/Integer;")
    }
  }

  "a class extending a nested class" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  class Foo {}
        |  class Bar extends Foo {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Bar").genericSignature.l shouldBe List("Ltest/Test$Foo;")
    }
  }

  "a class extending a local class" should {
    val cpg = code("""
        |class Test {
        |  public void test() {
        |    class Foo {}
        |    class Bar extends Foo {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      ???
    }
  }

  "a default constructor" should {
    val cpg = code("""
        |class Test {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("()V")
    }
  }

  "an explicit constructor" should {
    val cpg = code("""
        |class Test {
        |  public Test(String s) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(Ljava/lang/String;)V")
    }
  }

  "a compact constructor for a record" should {
    val cpg = code("""
        |record Test(String s) {
        |  public Test {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(Ljava/lang/String;)V")
    }
  }

  "a local with an unresolved fully qualified name" should {
    val cpg = code("""
        |class Test {
        |  public void test() {
        |    foo.Foo f;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("f").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a local with an unresolved type which can be inferred from imports" should {
    val cpg = code("""
        |import foo.Foo;
        |
        |class Test {
        |  public void test() {
        |    Foo f;
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("f").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a member with an unresolved fully qualified name" should {
    val cpg = code("""
        |class Test {
        |  foo.Foo f;
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.member.name("f").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a member with an unresolved type which can be inferred from imports" should {
    val cpg = code("""
        |import foo.Foo;
        |
        |class Test {
        |  Foo f;
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.member.name("f").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a method with an unresolved fully qualified return type and param" should {
    val cpg = code("""
        |class Test {
        |  public foo.Foo test(bar.Bar b) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(Lbar/Bar;)Lfoo/Foo;")
    }
  }

  "a method with an unresolved return type and param which can be inferred from imports" should {
    val cpg = code("""
        |import foo.Foo;
        |import bar.Bar;
        |
        |class Test {
        |  public Foo test(Bar b) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(Lbar/Bar;)Lfoo/Foo;")
    }
  }

  "a type decl extending an unresolved fully qualified type" should {
    val cpg = code("""
        |class Test extends foo.Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }

  "a type decl extending an unresolved type which can be inferred from imports" should {
    val cpg = code("""
        |import foo.Foo;
        |import bar.Bar;
        |
        |class Test extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo/Foo;")
    }
  }
}
