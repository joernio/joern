package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, TypeRef}
import io.shiftleft.semanticcpg.language.*

class ScopeTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |class Bar {
      |    public Object o = new Object();
      |    public static Object staticO = new Object();
      |
      |    public void bar() {}
      |    public static void staticBar() {}
      |}
      |
      |public class Test {
      |
      |    public Bar b;
      |
      |    public Object o = new Object();
      |    public static Object staticO = new Object();
      |
      |    public void foo() {}
      |
      |    public static void staticFoo() {}
      |
      |    /*********************************
      |     *             Calls             *
      |     *********************************/
      |    public void test1() {
      |        // Has `this` scope
      |        foo();
      |    }
      |
      |    public void test2() {
      |        // Has `this` scope
      |        this.foo();
      |    }
      |
      |    public void test3() {
      |        // No scope
      |        staticFoo();
      |    }
      |
      |    public void test4() {
      |        // No scope
      |        // TODO Fix in javasrc: Currently has scope node
      |        Test.staticFoo();
      |    }
      |
      |    public void test5() {
      |        // Field access to get node <- temp assignment
      |        // Temp node as scope
      |        // TODO Fix in javasrc: Has identifier instead of field access.
      |        b.bar();
      |    }
      |
      |    public void test6() {
      |        // No scope
      |        // TODO Fix in javasrc: Currently has scope node
      |        Bar.staticBar();
      |    }
      |
      |    public void test7() {
      |        // field access for system.out <- Temp assignment
      |        // Temp node as scope
      |        // TODO Fix in javasrc: Currently has identifier instead of field access
      |        System.out.println();
      |    }
      |
      |    /*********************************
      |     *         Field Accesses        *
      |     *********************************/
      |    public String test8() {
      |        // Field access for o
      |        // TODO Fix in javasrc: Currently has identifier instead of field access
      |        // Scope is a NameExpr
      |        // MethodName is a SimpleName
      |        return o.toString();
      |    }
      |
      |    public String test9() {
      |        // Identical to test8
      |        // TODO Fix in javasrc: Currently has identifier instead of field access
      |        // Scope is a fieldAccessExpr
      |        return this.o.toString();
      |    }
      |
      |    public String test10() {
      |        // Test.staticO
      |        // TODO Fix in javasrc: Currently has an identifier instead of field access
      |        // Same as test8
      |        return staticO.toString();
      |    }
      |
      |    public String test11() {
      |        // TODO Fix in javasrc? Currently has an identifier instead of field access
      |        // Scope is field access
      |        return Test.staticO.toString();
      |    }
      |
      |    public String test12() {
      |        // TODO Fix in javasrc.
      |        // Scope is single field access. We'll need to add another field access for `b`
      |        return b.o.toString();
      |    }
      |
      |    public String test13() {
      |        // Scope is a field access for Bar.staticO
      |        return Bar.staticO.toString();
      |    }
      |}
      |""".stripMargin)

  "it should create field access for simple non-static access as call scope" in {
    cpg.method.name("test8").call.name("toString").argument.l match {
      case List(fieldAccess: Call) =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.argument.l match {
          case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
            identifier.name shouldBe "this"
            fieldIdentifier.canonicalName shouldBe "o"
          case res => fail(s"Expected field access args but got $res")
        }

      case res => fail(s"Expected field access call but got $res")
    }
  }

  "it should create field access for simple non-static access with explicit this as call scope" in {
    cpg.method.name("test9").call.name("toString").argument.l match {
      case List(fieldAccess: Call) =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.argument.l match {
          case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
            identifier.name shouldBe "this"
            fieldIdentifier.canonicalName shouldBe "o"
          case res => fail(s"Expected field access args but got $res")
        }

      case res => fail(s"Expected field access call but got $res")
    }
  }

  "it should create field access for implicit static member as call scope" in {
    cpg.method.name("test10").call.name("toString").argument.l match {
      case List(fieldAccess: Call) =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.typeFullName shouldBe "java.lang.Object"
        fieldAccess.argument.l match {
          case List(typeRef: TypeRef, fieldIdentifier: FieldIdentifier) =>
            typeRef.typeFullName shouldBe "Test"
            fieldIdentifier.canonicalName shouldBe "staticO"
          case res => fail(s"Expected field access args but got $res")
        }

      case res => fail(s"Expected field access call but got $res")
    }
  }

  "it should create field access for explicit static member as call scope" in {
    cpg.method.name("test11").call.name("toString").argument.l match {
      case List(fieldAccess: Call) =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.argument.l match {
          case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
            identifier.name shouldBe "Test"
            fieldIdentifier.canonicalName shouldBe "staticO"
          case res => fail(s"Expected field access args but got $res")
        }

      case res => fail(s"Expected field access call but got $res")
    }
  }

  "it should create nested field accesses as call scope" in {
    cpg.method.name("test12").call.name("toString").argument.l match {
      case List(boAccess: Call) =>
        boAccess.methodFullName shouldBe Operators.fieldAccess
        boAccess.argument.l match {
          case List(bOnlyAccess: Call, oFieldIdentifier: FieldIdentifier) =>
            oFieldIdentifier.canonicalName shouldBe "o"

            bOnlyAccess.methodFullName shouldBe Operators.fieldAccess
            bOnlyAccess.argument.l match {
              case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
                identifier.name shouldBe "this"
                fieldIdentifier.canonicalName shouldBe "b"

              case res => fail(s"Expected identifier and field Identifier for this.b access but got $res")
            }

          case res => fail(s"Expected field access call but got $res")
        }
      case res => fail(s"Expected b.o field access but got $res")
    }
  }
}
