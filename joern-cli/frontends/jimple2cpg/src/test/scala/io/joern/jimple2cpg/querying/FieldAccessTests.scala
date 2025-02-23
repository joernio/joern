package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends JimpleCode2CpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  val cpg: Cpg = code("""
      |class Foo {
      |  public static int MAX_VALUE = 12;
      |  public int value;
      |
      |  public Foo(int value) {
      |    if (value <= MAX_VALUE) {
      |      this.value = value;
      |    } else {
      |      this.value = MAX_VALUE;
      |    }
      |  }
      |
      |  public void setValue(int value) {
      |    if (value <= MAX_VALUE) {
      |      this.value = value;
      |    }
      |  }
      |}
      |
      |class Test {
      |public void foo() {
      |  int x = Foo.MAX_VALUE;
      |}
      |
      |public void bar() {
      |  Foo f = new Foo(5);
      |  int y = f.value;
      |}
      |
      |public void baz() {
      |  Foo g = new Foo(5);
      |  g.value = 66;
      |}
      |}
      |""".stripMargin).cpg

  "should handle static member accesses" in {
    val List(assign: Call) = cpg.method(".*foo.*").call(".*assignment").l
    assign.code shouldBe "x = Foo.MAX_VALUE"
    val List(access: Call) = cpg.method(".*foo.*").call(".*fieldAccess").l
    access.code shouldBe "Foo.MAX_VALUE"
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.code shouldBe "Foo"
    identifier.name shouldBe "Foo"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "MAX_VALUE"
    fieldIdentifier.code shouldBe "MAX_VALUE"
  }

  "should handle object field accesses on RHS of assignments" in {
    val List(_: Call, _: Call, assign: Call) = cpg.method(".*bar.*").call(".*assignment").l
    assign.code shouldBe "y = f.value"
    val List(access: Call)                                             = cpg.method(".*bar.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "f"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  "should handle object field accesses on LHS of assignments" in {
    val List(_: Call, _: Call, assign: Call) = cpg.method(".*baz.*").call(".*assignment").l
    assign.code shouldBe "g.value = 66"
    val List(access: Call)                                             = cpg.method(".*baz.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "g"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  val cpg2 = code("""
    |import java.util.Objects;
    |class TestStaticFinal {
    |    private static final String finalPrivateString = "PRIVATE_STATIC_FINAL";
    |    public static final String finalPublicString = "PUBLIC_STATIC_FINAL";
    |    public String publicString;
    |    public static String staticString = "PUBLIC_STATIC";
    |    private static final Object obj = new Object();
    |    public static final int FLAG = 1;
    | public static int staticInt = 100;
    |    public TestStaticFinal(String publicString) {
    |        if (Objects.equals(publicString, "")) {
    |            this.publicString = finalPrivateString+"-"+ staticString+"-"+ finalPublicString;
    |        }else{
    |            this.publicString = publicString;
    |        }
    |    }
    |    public void setPublicString(String publicString) {
    |        this.publicString = publicString;
    |    }
    |    public String getFinal(){
    |        return finalPublicString.concat(finalPrivateString.toLowerCase());
    |    }
    |    public String getPublicString(){
    |        return publicString;
    |    }
    |}
    |class Test {
    |    public static void test(){
    |        TestStaticFinal test = new TestStaticFinal("");
    |        String x = test.getPublicString();
    |        System.out.println(x);
    |        test.setPublicString("Change");
    |        String y = test.getPublicString() + "|" + TestStaticFinal.staticString+ "|"+ TestStaticFinal.finalPublicString;
    |        System.out.println(y);
    |        String finalS = test.getFinal();
    |        System.out.println(finalS);
    |    }
    |}
    |""".stripMargin).cpg

  "should handle static final field access" in {
    val staticFields = cpg2.typeDecl.name("TestStaticFinal").member.l
    println("Static fields:")
    staticFields.foreach(f => println(s"${f.name}: ${f.code}"))

    val clinitAssignments = cpg2.method("<clinit>").call(".*assignment").code.l
    println("Clinit assignments:")
    clinitAssignments.foreach(println)
    val staticStringAssignment = clinitAssignments.find(_.contains("staticString")).getOrElse(null)
    staticStringAssignment shouldBe "TestStaticFinal.staticString = \"PUBLIC_STATIC\""

    val finalPrivateStringField = staticFields.find(_.name == "finalPrivateString").get
    finalPrivateStringField.code shouldBe "java.lang.String finalPrivateString = \"PRIVATE_STATIC_FINAL\""
    val finalPublicStringField = staticFields.find(_.name == "finalPublicString").get
    finalPublicStringField.code shouldBe "java.lang.String finalPublicString = \"PUBLIC_STATIC_FINAL\""
    val staticStringField = staticFields.find(_.name == "staticString").get
    staticStringField.code shouldBe "java.lang.String staticString"
    val finalFLAGField = staticFields.find(_.name == "FLAG").get
    finalFLAGField.code shouldBe "int FLAG = 1"
  }
}
