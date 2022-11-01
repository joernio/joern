package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class GenericsTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |import java.util.function.Function;
      |
      |// Box
      |class Box<T> {
      |
      |    // java.lang.Object
      |    private T item;
      |
      |    // Box.getItem:java.lang.Object()
      |    public T getItem() {
      |        return item;
      |    }
      |
      |    public void setItem(T item) {
      |        this.item = item;
      |    }
      |
      |    // Box.map:Box(java.util.function.Function)
      |    public <G> Box<G> map(Function<T, G> f) {
      |        // java.util.function.Function.apply: java.lang.Object(java.lang.Object)
      |        G newValue = f.apply(item);
      |        // Box.<init>:void()
      |        Box<G> newBox = new Box<G>();
      |        return newBox.withValue(newValue);
      |    }
      |
      |    // Box.withValue:Box(java.lang.Object)
      |    public Box<T> withValue(T value) {
      |        this.item = value;
      |        return this;
      |    }
      |
      |    public String toString() {
      |        return "Box(" + item.toString() + ")";
      |    }
      |
      |    // Box.idk:java.lang.Number(java.lang.Number)
      |    public static <K extends Number> K idK(K item) {
      |        return item;
      |    }
      |
      |    // Box.idKC:java.lang.Number(java.lang.Number)
      |    public static <K extends Number & Comparable> K idKC(K item) {
      |        return item;
      |    }
      |
      |    // Box.idC:java.lang.Comparable(java.lang.Comparable)
      |    public static <K extends Comparable> K idC(K item) {
      |        return item;
      |    }
      |
      |    // Box.testWildCard:void(Box)
      |    public static void testWildCard(Box<? extends Comparable> b) {
      |        System.out.println(b);
      |    }
      |
      |    // Box.testWildCardLower:void(Box)
      |    public static void testWildCardLower(Box<? super Integer> b) {
      |        System.out.println(b);
      |    }
      |}
      |
      |
      |// inheritsFrom Box
      |public class Test extends Box<String> {}
      |""".stripMargin)

  "it should create the correct generic typeDecl name" in {
    cpg.typeDecl.nameExact("Box").l match {
      case decl :: Nil => decl.fullName shouldBe "Box"

      case res => fail(s"Expected typeDecl Box but got $res")
    }
  }

  "it should default to Object for a simple generic type" in {
    cpg.method.name("getItem").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.getItem:java.lang.Object()"
        method.signature shouldBe "java.lang.Object()"

      case res => fail(s"Expected method getItem but got $res")
    }
  }

  "it should default to Object for simple generic parameters" in {
    cpg.method.name("setItem").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.setItem:void(java.lang.Object)"
        method.signature shouldBe "void(java.lang.Object)"

      case res => fail(s"Expected method setItem but got $res")
    }

    cpg.method.name("setItem").parameter.name("item").l match {
      case node :: Nil =>
        node.typeFullName shouldBe "java.lang.Object"

      case res => fail(s"Expected param item but got $res")
    }
  }

  "it should erase generic types in parameters" in {
    val List(method) = cpg.method.name("map").l
    method.fullName shouldBe "Box.map:Box(java.util.function.Function)"
    method.signature shouldBe "Box(java.util.function.Function)"

    val List(param) = cpg.method.name("map").parameter.name("f").l
    param.typeFullName shouldBe "java.util.function.Function"
  }

  "it should create correct constructor calls" in {
    cpg.method.name("map").call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l match {
      case const :: Nil =>
        const.methodFullName shouldBe s"Box.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
        const.signature shouldBe "void()"

      case res => fail(s"Expected call to <init> but got $res")
    }
  }

  "it should correctly handle generic return types" in {
    cpg.method.name("withValue").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.withValue:Box(java.lang.Object)"
        method.signature shouldBe "Box(java.lang.Object)"

      case res => fail(s"Expected method withValue but got $res")
    }
  }

  "it should handle generics with upper bounds" in {
    cpg.method.name("idK").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.idK:java.lang.Number(java.lang.Number)"
        method.signature shouldBe "java.lang.Number(java.lang.Number)"

      case res => fail(s"Expected method idK but found $res")
    }
  }

  "it should handle generics with compound upper bounds" in {
    cpg.method.name("idKC").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.idKC:java.lang.Number(java.lang.Number)"
        method.signature shouldBe "java.lang.Number(java.lang.Number)"

      case res => fail(s"Expected method idKC but found $res")
    }
  }

  "it should handle generics with an interface upper bound" in {
    cpg.method.name("idC").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.idC:java.lang.Comparable(java.lang.Comparable)"
        method.signature shouldBe "java.lang.Comparable(java.lang.Comparable)"

      case res => fail(s"Expected method idC but found $res")
    }
  }

  "it should handle wildcard subclass generics" in {
    cpg.method.name("testWildCard").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.testWildCard:void(Box)"
        method.signature shouldBe "void(Box)"

      case res => fail(s"Expected method testWildCard but found $res")
    }
  }

  "it should handle wildcard superclass generics" in {
    cpg.method.name("testWildCardLower").l match {
      case method :: Nil =>
        method.fullName shouldBe "Box.testWildCardLower:void(Box)"
        method.signature shouldBe "void(Box)"

      case res => fail(s"Expected method testWildCardLower but found $res")
    }
  }

  "it should handle generic inheritance" in {
    cpg.typeDecl.name("Test").l match {
      case decl :: Nil =>
        decl.inheritsFromTypeFullName.head shouldBe "Box"

      case res => fail(s"Expected typeDecl Test but found $res")
    }
  }

}
