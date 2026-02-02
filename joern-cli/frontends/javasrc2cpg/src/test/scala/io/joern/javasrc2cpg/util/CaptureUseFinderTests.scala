package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Inside

/** Tests for CaptureUseFinder edge cases identified in code reviews.
  *
  * Known bugs (currently failing):
  *   - Explicit ThisExpr not captured (this, this.field, this.method())
  *   - SuperExpr not captured
  *   - Qualified this (Outer.this) not properly distinguished
  *
  * Working correctly:
  *   - TryStmt resources scoping
  *   - LocalRecordDeclarationStmt scoping
  *   - Pattern matching in loop conditions
  *   - Fields of local classes not leaked
  *   - Instance initializers scoping
  */
class CaptureUseFinderTests extends JavaSrcCode2CpgFixture with Inside {

  // ============================================================================
  // Review 1: Core Issues
  // ============================================================================

  "explicit ThisExpr expressions" should {
    val cpg = code("""
        |public class Foo {
        |  public void test() {
        |    Runnable r = () -> {
        |      System.out.println(this);
        |    };
        |  }
        |}
        |""".stripMargin)

    "create a closure binding for explicit this reference" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined

          inside(cpg.closureBinding.filter(_.closureBindingId == thisLocal.closureBindingId).l) { case List(binding) =>
            inside(binding._refOut.collectAll[MethodParameterIn].l) { case List(thisParam) =>
              thisParam.name shouldBe "this"
              thisParam.method.name shouldBe "test"
            }
          }
        }
      }
    }
  }

  "explicit this.field access in lambda" should {
    val cpg = code("""
        |public class Foo {
        |  private String value = "test";
        |
        |  public void test() {
        |    Runnable r = () -> {
        |      System.out.println(this.value);
        |    };
        |  }
        |}
        |""".stripMargin)

    "create a closure binding for this when accessing this.field" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined
        }
      }
    }
  }

  "explicit this.method() call in lambda" should {
    val cpg = code("""
        |public class Foo {
        |  private void helper() {}
        |
        |  public void test() {
        |    Runnable r = () -> {
        |      this.helper();
        |    };
        |  }
        |}
        |""".stripMargin)

    "create a closure binding for this when calling this.method()" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined
        }
      }
    }
  }

  "SuperExpr expressions" should {
    val cpg = code("""
        |class Base {
        |  public String getValue() { return "base"; }
        |}
        |
        |class Foo extends Base {
        |  public void test() {
        |    Runnable r = () -> {
        |      System.out.println(super.getValue());
        |    };
        |  }
        |}
        |""".stripMargin)

    "create a closure binding for this when using super" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined
        }
      }
    }
  }

  "TryStmt with resources" should {
    val cpg = code("""
        |import java.io.*;
        |
        |public class Foo {
        |  public void test(String path) {
        |    Runnable r = () -> {
        |      try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
        |        System.out.println(reader.readLine());
        |      } catch (IOException e) {
        |        // ignore
        |      }
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture path but not reader (which is declared in try-with-resources)" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        capturedLocals should contain("path")
        capturedLocals should not contain "reader"
      }
    }
  }

  "TryStmt with resources shadowing a captured field" should {
    val cpg = code("""
        |import java.io.*;
        |
        |public class Test {
        |  BufferedReader reader;
        |
        |  public void test(String path) {
        |    Runnable r = () -> {
        |      try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
        |        System.out.println(reader);
        |      } catch (IOException e) {
        |        // ignore
        |      }
        |      System.out.println(reader);
        |    };
        |
        |    r.run();
        |  }
        |}
        |""".stripMargin)

    "correctly handle the scope of the variable defined in the try with resources" in {
      inside(cpg.method.name(".*lambda.*").call.name("println").sortBy(_.order).argument.argumentIndex(1).l) {
        case List(localReader: Identifier, capturedReader: Call) =>
          localReader.name shouldBe "reader"
          inside(localReader.refsTo.l) { case List(readerLocal: Local) =>
            readerLocal.name shouldBe "reader"
            readerLocal.method.fullName.l shouldBe List("Test.<lambda>0:void()")
            readerLocal.closureBindingId shouldBe None
          }

          capturedReader.name shouldBe Operators.fieldAccess
          inside(capturedReader.argument.l) { case List(thisId: Identifier, readerFieldId: FieldIdentifier) =>
            thisId.name shouldBe "this"
            thisId.typeFullName shouldBe "Test"
            inside(thisId.refsTo.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
            }

            readerFieldId.canonicalName shouldBe "reader"
          }
      }
    }
  }

  "LocalRecordDeclarationStmt" should {
    val cpg = code("""
        |public class Foo {
        |  public void test(String captured) {
        |    Runnable r = () -> {
        |      record LocalRecord(String value) {
        |        void print() {
        |          System.out.println(value);
        |        }
        |      }
        |      new LocalRecord(captured).print();
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture captured but not value (which is a record component)" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        capturedLocals should contain("captured")
        capturedLocals should not contain "value"
      }
    }
  }

  // TODO Ignored until support for LocalRecordDeclarationStmt is added
  "LocalRecordDeclarationStmt shadowing a field capture" ignore {
    val cpg = code("""
                     |public class Foo {
                     |  String value;
                     |  public void test(String captured) {
                     |    Runnable r = () -> {
                     |      record LocalRecord(String value) {
                     |        void print() {
                     |          System.out.println(value);
                     |        }
                     |      }
                     |      System.out.println(value);
                     |      new LocalRecord(captured).print();
                     |    };
                     |  }
                     |}
                     |""".stripMargin)

    "correctly handle the scope of the local record parameter in the lambda" in {
      inside(cpg.method.name(".*lambda.*").call.name("println").argument.argumentIndex(1).l) {
        case List(capturedValue: Call) =>
          capturedValue.name shouldBe Operators.fieldAccess
          inside(capturedValue.argument.l) { case List(thisId: Identifier, valueFieldId: FieldIdentifier) =>
            thisId.name shouldBe "this"
            thisId.typeFullName shouldBe "Foo"
            inside(thisId.refsTo.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.closureBindingId shouldBe Some("Test0.java:Foo.<lambda>0:this")
            }

            valueFieldId.canonicalName shouldBe "value"
          }
      }
    }

    "correctly handle the scope of the local record parameter in the local record" in {
      inside(cpg.method.name("print").call.name("println").argument.argumentIndex(1).l) {
        case List(localValue: Call, capturedReader: Call) =>
          localValue.name shouldBe Operators.fieldAccess
          inside(localValue.argument.l) { case List(thisId: Identifier, valueFieldId: FieldIdentifier) =>
            thisId.name shouldBe "this"
            thisId.typeFullName shouldBe "Foo.test.LocalRecord"
            inside(thisId.refsTo.l) { case List(thisParam: MethodParameterIn) =>
              thisParam.name shouldBe "this"
              thisParam.index shouldBe 0
              thisParam.method.fullName shouldBe "Foo.test.LocalRecord.print:void(java.lang.String)"
            }
          }
      }
    }
  }

  // ============================================================================
  // Review 2: Edge Cases and Complex Scenarios
  // ============================================================================

  "pattern matching in while loop condition" should {
    val cpg = code("""
        |public class Foo {
        |  public void test(Object obj) {
        |    Runnable r = () -> {
        |      while (obj instanceof String s) {
        |        System.out.println(s);
        |        break;
        |      }
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture obj but not s (which is declared by pattern matching)" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        capturedLocals should contain("obj")
        capturedLocals should not contain "s"
      }
    }
  }

  "pattern matching in while loop condition shadowing a field capture" should {
    val cpg = code("""
                     |public class Foo {
                     |  String s;
                     |  public void test(Object obj) {
                     |    Runnable r = () -> {
                     |      while (obj instanceof String s) {
                     |        System.out.println(s);
                     |        break;
                     |      }
                     |      System.out.println(s);
                     |    };
                     |  }
                     |}
                     |""".stripMargin)

    "capture obj but not s (which is declared by pattern matching)" in {
      // cpg.method.name("*.lambda.*").call doesn't return calls in the desired order, but fortunately sorting the
      // println calls by the order field coincidentally gives the right permutation
      inside(cpg.method.name(".*lambda.*").call.name("println").sortBy(_.order).argument.argumentIndex(1).l) {
        case List(localS: Identifier, capturedS: Call) =>
          localS.name shouldBe "s"
          inside(localS.refsTo.l) { case List(sLocal: Local) =>
            sLocal.name shouldBe "s"
            sLocal.closureBindingId shouldBe None
          }

          capturedS.name shouldBe Operators.fieldAccess
          inside(capturedS.argument.l) { case List(thisId: Identifier, sFieldId: FieldIdentifier) =>
            thisId.name shouldBe "this"
            thisId.typeFullName shouldBe "Foo"
            inside(thisId.refsTo.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.typeFullName shouldBe "Foo"
              thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
            }

            sFieldId.canonicalName shouldBe "s"
          }
      }
    }
  }

  "pattern matching in for loop condition" should {
    val cpg = code("""
        |import java.util.*;
        |
        |public class Foo {
        |  public void test(List<Object> items) {
        |    Runnable r = () -> {
        |      for (Object item : items) {
        |        if (item instanceof String s) {
        |          System.out.println(s);
        |        }
        |      }
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture items but not s or item" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        capturedLocals should contain("items")
        capturedLocals should not contain "item"
        capturedLocals should not contain "s"
      }
    }
  }

  "pattern variables introduced in binary expressions" should {
    val cpg = code("""
        |public class Test {
        |  String s = "sField";
        |
        |  static boolean isValid(String s) {
        |    System.out.println(s);
        |    return false;
        |}
        |
        |  public void test(Object obj) {
        |    Runnable r = () -> {
        |      while ((obj instanceof String s && isValid(s)) || isValid(s)) {
        |        break;
        |      }
        |    };
        |    r.run();
        |  }
        |
        |  public static void main(String[] args) {
        |    new Test().test("capturedParam");
        |  }
        |}
        |
        |""".stripMargin)

    "be handled in the correct scope" in {
      inside(cpg.call.nameExact(Operators.logicalOr).argument.l) { case List(leftCondition, rightCondition) =>
        inside(leftCondition.ast.isCallTo("isValid").argument.l) { case List(localS: Identifier) =>
          localS.name shouldBe "s"
          inside(localS.refsTo.l) { case List(sLocal: Local) =>
            sLocal.name shouldBe "s"
            sLocal.closureBindingId shouldBe None
          }
        }

        inside(rightCondition.ast.isCallTo("isValid").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          inside(fieldAccess.argument.l) { case List(thisId: Identifier, sFieldId: FieldIdentifier) =>
            thisId.name shouldBe "this"
            inside(thisId.refsTo.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.typeFullName shouldBe "Test"
              thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
            }

            sFieldId.canonicalName shouldBe "s"
          }
        }
      }
    }
  }

  "fields of local classes" should {
    val cpg = code("""
        |public class Foo {
        |  public void test() {
        |    Runnable r = () -> {
        |      class Local {
        |        int field = 42;
        |        void method() {
        |          System.out.println(field);
        |        }
        |      }
        |      new Local().method();
        |    };
        |  }
        |}
        |""".stripMargin)

    "not capture field (which is a field of the local class)" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        capturedLocals should not contain "field"
      }
    }
  }

  // TODO Fix this when fixing qualified this references
  "qualified this references (Outer.this)" ignore {
    val cpg = code("""
        |public class Outer {
        |  private String outerValue = "outer";
        |
        |  public void method() {
        |    Runnable r = () -> {
        |      class Inner {
        |        void innerMethod() {
        |          System.out.println(Outer.this.outerValue);
        |        }
        |      }
        |      new Inner().innerMethod();
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture Outer.this for the lambda" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined
        }
      }
    }
  }

  "instance initializers in anonymous classes" should {
    val cpg = code("""
        |public class Foo {
        |  private String value = "outer";
        |
        |  public void test() {
        |    Runnable r = () -> {
        |      Object obj = new Object() {
        |        {
        |          System.out.println(this);
        |        }
        |      };
        |    };
        |  }
        |}
        |""".stripMargin)

    "not leak this from instance initializer to outer lambda" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        val capturedLocals = lambdaMethod.local.filter(_.closureBindingId.isDefined).name.toSet

        // `this` inside the initializer refers to the anonymous Object, not Foo
        // so the lambda should NOT capture `this` just because of the initializer
        capturedLocals should not contain "this"
      }
    }
  }

  "instance initializer accessing outer field" should {
    val cpg = code("""
        |public class Foo {
        |  private String value = "outer";
        |
        |  public void test() {
        |    Runnable r = () -> {
        |      Object obj = new Object() {
        |        {
        |          System.out.println(value);
        |        }
        |      };
        |    };
        |  }
        |}
        |""".stripMargin)

    "capture this when instance initializer accesses outer field" in {
      inside(cpg.method.name(".*lambda.*").l) { case List(lambdaMethod) =>
        inside(lambdaMethod.local.name("this").l) { case List(thisLocal) =>
          thisLocal.closureBindingId shouldBe defined

          inside(cpg.closureBinding.filter(_.closureBindingId == thisLocal.closureBindingId).l) { case List(binding) =>
            inside(binding._refOut.collectAll[MethodParameterIn].l) { case List(thisParam) =>
              thisParam.name shouldBe "this"
              thisParam.method.name shouldBe "test"
            }
          }
        }
      }
    }
  }
}
