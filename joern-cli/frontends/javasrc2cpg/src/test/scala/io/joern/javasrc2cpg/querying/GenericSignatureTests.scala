package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

/** ### Class type signatures
  *   - In most cases, only the simple name for the class will be used (so `LString;` will be used instead of
  *     `Ljava/lang/String`)
  *
  *   - Where a qualified name is used in source, that name is used verbatim in the signature, for example
  *     `Ljava.util.List` (note the `.` were not substituted for `/`.
  *
  *   - For local classes, the name of the class as it appears in the CPG is used in the signature for instances of that
  *     class (we don't follow the JVM naming scheme for these), for example
  *     `Ltestpackage.TestClass.testMethod.LocalClass;`
  *
  * ### Type parameter bounds From the language specification:
  * ```
  * TypeParameter:
  *  Identifier ClassBound {InterfaceBound}
  *
  *  ClassBound:
  *  : [ReferenceTypeSignature]
  *
  *  InterfaceBound:
  *  : ReferenceTypeSignature
  * ```
  * If a type parameter only has interface bounds I1, I2, ..., then the signature should contain `<T::LI1;:...>` (note
  * the empty class bound), but in general we won't know if a type is a class or interface without resolving the it, so
  * the signature in the CPG will contain `<T:LI1;:...>` instead.
  *
  * ### Unspecified types Where no type name is specified, the special `L__unspecified_type;` type is used in generic
  * signatures. This happens in a few places:
  *   - For lambda return types and lambda parameters which do not have explicit type annotations
  *
  *   - For lambda type decls
  *
  *   - For locals with a `var` type, for example `var x = 42`
  *
  *   - For synthetic locals created for `foreach` loops, for example in `for (String item : items())`, we create a
  *     temporary `String[] $iterLocal0 = items()` local which will have an unspecified signature (`item` will still
  *     have the signature `LString;` as expected)
  *
  *   - For synthetic locals created for the LHS of `instanceof` expressions with pattern matching, for example in
  *     `foo() instanceof String s`, we create an `Object o = foo()` local (since the type depends on the return type of
  *     `foo`).
  */
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
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LObject;")
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
      cpg.method.name("test").genericSignature.l shouldBe List("(LTest;LInteger;)LString;")
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
      cpg.method.name("test").genericSignature.l shouldBe List("(LTest;)LFoo;")
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
      cpg.method.name("test").genericSignature.l shouldBe List("(LFoo;)V")
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
        |import foo.Foo;
        |
        |class Test extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LFoo;")
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
        |import foo.Foo;
        |
        |class Test implements Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LObject;LFoo;")
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
        |import foo.Foo;
        |import bar.Bar;
        |
        |class Test extends Foo implements Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LFoo;LBar;")
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
        |import foo.Foo;
        |import bar.Bar;
        |
        |class Test implements Foo, Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LObject;LFoo;LBar;")
    }
  }

  "an interface not extending another interface" should {
    val cpg = code("""
        |package foo;
        |
        |interface Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Foo").genericSignature.l shouldBe List("LObject;")
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
        |import foo.Foo;
        |
        |interface Bar extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Bar").genericSignature.l shouldBe List("LObject;LFoo;")
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
        |import foo.Foo;
        |import bar.Bar;
        |
        |interface Test extends Foo, Bar {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LObject;LFoo;LBar;")
    }
  }

  "a class extending an unresolved class" should {
    val cpg = code("""
        |package test;
        |
        |class Test extends Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LFoo;")
    }
  }

  "a class implementing an unresolved interface" should {
    val cpg = code("""
        |package test;
        |
        |class Test implements Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LObject;LFoo;")
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
      cpg.method.name(".*lambda.*").genericSignature.l shouldBe List("(L__unspecified_type;)L__unspecified_type;")
    }

    "have an empty generic signature for the lambda type decl" in {
      cpg.typeDecl.name(".*lambda.*").genericSignature.l shouldBe List("L__unspecified_type;")
    }
  }

  "a lambda method with an explicit type annotation" should {
    val cpg = code("""
                     |package test;
                     |
                     |import java.util.function.Consumer;
                     |
                     |class Test {
                     |  public Consumer<String> test() {
                     |    return (String s) -> System.out.println(s);
                     |  }
                     |}
                     |""".stripMargin)

    "have the correct generic signature for the lambda method" in {
      cpg.method.name(".*lambda.*").genericSignature.l shouldBe List("(LString;)L__unspecified_type;")
    }

    "have an empty generic signature for the lambda type decl" in {
      cpg.typeDecl.name(".*lambda.*").genericSignature.l shouldBe List("L__unspecified_type;")
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
      cpg.method.name(".*lambda.*").genericSignature.l shouldBe List("(L__unspecified_type;)L__unspecified_type;")
    }

    "have an empty generic signature for the lambda type decl" in {
      cpg.typeDecl.name(".*lambda.*").genericSignature.l shouldBe List("L__unspecified_type;")
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
      cpg.typeDecl.nameExact("Test$Nested").genericSignature.l shouldBe List("LObject;")
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
      cpg.typeDecl.name("Local").genericSignature.l shouldBe List("LObject;")
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
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("LFoo;")
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
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("LFoo;")
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
      cpg.typeDecl.nameExact("Foo$0").genericSignature.l shouldBe List("LFoo;")
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
      cpg.local.name("items").genericSignature.l shouldBe List("[LString;")
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
      cpg.local.name("list").genericSignature.l shouldBe List("LList<LString;>;")
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
      cpg.local.name("list").genericSignature.l shouldBe List("LList<*>;")
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
      cpg.local.name("list").genericSignature.l shouldBe List("LList<+LString;>;")
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
      cpg.local.name("list").genericSignature.l shouldBe List("LList<-LString;>;")
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
      cpg.local.name("map").genericSignature.l shouldBe List("LMap<LString;LInteger;>;")
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
      cpg.local.name("map").genericSignature.l shouldBe List("LMap<LString;LList<LString;>;>;")
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
      cpg.local.name("list").genericSignature.l shouldBe List("LList<TS;>;")
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
      cpg.local.name("t").genericSignature.l shouldBe List("LList<+TT;>;")
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
      cpg.method.name("test").genericSignature.l shouldBe List("<S:LObject;T:LList;>(TT;)TS;")
    }
  }

  "a type parameter with multiple interface bounds" should {
    val cpg = code("""
        |package test;
        |
        |interface I1 {}
        |interface I2 {}
        |
        |class Test {
        |  public <T extends I1 & I2> void test(T t) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("<T:LI1;:LI2;>(TT;)V")
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
      cpg.member.name("list").genericSignature.l shouldBe List("LList<LString;>;")
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
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LEnum<LTest;>;")
    }

    "have the correct generic signature for the enum constant" in {
      cpg.member.name("TEST").genericSignature.l shouldBe List("LTest;")
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
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("<T:LObject;>LRecord;")
    }

    "have the correct generic signature for the record parameter fields" in {
      cpg.member.name("value").genericSignature.l shouldBe List("LString;")
      cpg.member.name("list").genericSignature.l shouldBe List("LList<TT;>;")
    }

    "have the correct generic signature for the default constructor" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(LString;LList<TT;>;)V")
    }

    "have the correct generic signature for the record paramater accessors" in {
      cpg.method.name("value").genericSignature.l shouldBe List("()LString;")
      cpg.method.name("list").genericSignature.l shouldBe List("()LList<TT;>;")
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
      cpg.typeDecl.name("Bar").genericSignature.l shouldBe List("<T:LObject;>LObject;")
    }

    "have the correct generic signature for the inheriting class" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LBar<LString;>;")
    }
  }

  "the lowering for a native foreach loop with a synthetic iterator local" should {
    val cpg = code("""
                     |package test;
                     |
                     |class Test {
                     |  String[] items() { return null; }
                     |  void test() {
                     |    for (String item : items()) {}
                     |  }
                     |}
                     |""".stripMargin)

    "have the correct generic signature for the synthetic iterator local" in {
      cpg.local.nameExact("$iterLocal0").genericSignature.l shouldBe List("L__unspecified_type;")
    }

    "have the correct generic signature for the synthetic index local" in {
      cpg.local.nameExact("$idx0").genericSignature.l shouldBe List("I")
    }

    "have the correct generic signature for the variable local" in {
      cpg.local.name("item").genericSignature.l shouldBe List("LString;")
    }
  }

  "the lowering for a native foreach loop" should {
    val cpg = code("""
        |package test;
        |
        |class Test {
        |  void test(String[] items) {
        |    for (String item : items) {}
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature for the synthetic index local" in {
      cpg.local.nameExact("$idx0").genericSignature.l shouldBe List("I")
    }

    "have the correct generic signature for the variable local" in {
      cpg.local.name("item").genericSignature.l shouldBe List("LString;")
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
      cpg.local.nameExact("$iterLocal0").genericSignature.l shouldBe List("Ljava.util.Iterator;")
    }

    "have the correct generic signature for the variable local" in {
      cpg.local.name("item").genericSignature.l shouldBe List("LString;")
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
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name.foreach(println)
      cpg.local.nameExact("$obj0").genericSignature.l shouldBe List("LString;")
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
      cpg.local.name("captured").genericSignature.l shouldBe List("LInteger;")
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
      cpg.local.nameExact("$obj0").genericSignature.l shouldBe List("L__unspecified_type;")
    }

    "have the correct generic signature for the pattern variable local" in {
      cpg.local.name("s").genericSignature.l shouldBe List("LString;")
    }
  }

  "a local class with captures" should {
    val cpg = code("""
        |class Test<T> {
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
      // TODO: This should maybe be `LTest<TT;>;` instead, but the type variable <T> has no
      //  meaning in `Foo`.
      cpg.member.name("outerClass").genericSignature.l shouldBe List("LTest;")
    }

    "have the correct generic signature for a captured parameter member" in {
      cpg.member.name("testParam").genericSignature.l shouldBe List("LInteger;")
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
      cpg.typeDecl.nameExact("Test$Bar").genericSignature.l shouldBe List("LTest$Foo;")
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
      cpg.typeDecl.nameExact("Bar").genericSignature.l shouldBe List("LTest.test.Foo;")
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
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(LString;)V")
    }
  }

  "a compact constructor for a record" should {
    val cpg = code("""
        |record Test(String s) {
        |  public Test {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.nameExact("<init>").genericSignature.l shouldBe List("(LString;)V")
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
      cpg.local.name("f").genericSignature.l shouldBe List("Lfoo.Foo;")
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
      cpg.local.name("f").genericSignature.l shouldBe List("LFoo;")
    }
  }

  "a member with an unresolved fully qualified name" should {
    val cpg = code("""
        |class Test {
        |  foo.Foo f;
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.member.name("f").genericSignature.l shouldBe List("Lfoo.Foo;")
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
      cpg.member.name("f").genericSignature.l shouldBe List("LFoo;")
    }
  }

  "a method with an unresolved fully qualified return type and param" should {
    val cpg = code("""
        |class Test {
        |  public foo.Foo test(bar.Bar b) {}
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.method.name("test").genericSignature.l shouldBe List("(Lbar.Bar;)Lfoo.Foo;")
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
      cpg.method.name("test").genericSignature.l shouldBe List("(LBar;)LFoo;")
    }
  }

  "a type decl extending an unresolved fully qualified type" should {
    val cpg = code("""
        |class Test extends foo.Foo {}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("Lfoo.Foo;")
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
      cpg.typeDecl.name("Test").genericSignature.l shouldBe List("LFoo;")
    }
  }

  "a local with a var type" should {
    val cpg = code("""
        |public class Test {
        |  public void foo() {
        |    var s = "hello";
        |  }
        |}
        |""".stripMargin)

    "have the correct generic signature" in {
      cpg.local.name("s").genericSignature.l shouldBe List("L__unspecified_type;")
    }
  }
}
