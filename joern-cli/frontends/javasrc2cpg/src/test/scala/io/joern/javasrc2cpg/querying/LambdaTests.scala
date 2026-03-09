package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class LambdaTests extends JavaSrcCode2CpgFixture {
  "unresolved lambda parameters should have an ANY type" in {
    val cpg = code("""
        |public class Test {
        |  public void method() {
        |    unresolvedCall().foreach(lambdaParam -> {
        |       foo(lambdaParam);
        |    });
        |  }
        |}
        |""".stripMargin)

    cpg.call.name("foo").argument.collectAll[Identifier].name("lambdaParam").typeFullName.toList shouldBe List("ANY")
  }

  "nested lambdas" should {
    val cpg = code("""
        |import java.util.ArrayList;
        |import java.util.List;
        |import java.util.stream.Collectors;
        |
        |public class TestClass {
        |  public Integer method(Integer aaa) {
        |    List<Integer> list = new ArrayList<>();
        |    list.add(1);
        |
        |    List<Integer> mappedList = list.stream().map(integer -> {
        |      List<Integer> nestedList = new ArrayList<>();
        |      nestedList.add(1);
        |
        |      List<Integer> nestedMappedList =
        |          nestedList.stream().map(nestedInteger -> nestedInteger + aaa).collect(Collectors.toList());
        |      return nestedMappedList.get(0);
        |    }).collect(Collectors.toList());
        |    Integer ret = mappedList.get(0);
        |    return ret;
        |  }
        |}
        |""".stripMargin)

    "create 2 method nodes for the respective lambdas" in {
      cpg.method.name(".*lambda.*").name.l should contain theSameElementsAs List("<lambda>0", "<lambda>1")
    }
  }

  "lambdas used as a function argument" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public static String getFromSupplier(String input, Function<String, String> mapper) {
        |    return mapper.apply(input);
        |  }
        |
        |  public void test1(String input, String fallback) {
        |    getFromSupplier(
        |      input,
        |      lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback
        |    );
        |  }
        |}
        |""".stripMargin)

    "create the correct type node for the lambda" in {
      cpg.typ.name(".*lambda.*").name.l shouldBe List("<lambda>0")
      cpg.typ.name(".*lambda.*").fullName.l shouldBe List("Foo.<lambda>0:java.lang.String(java.lang.String)")
    }

    "create the correct methodref at the lambda callsite" in {
      inside(cpg.call.name("getFromSupplier").argument.argumentIndex(2).l) { case List(methodRef: MethodRef) =>
        methodRef.methodFullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
        methodRef.typeFullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
      }
    }

    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "not create a binding for the lambda method" in {
      cpg.all.collectAll[Binding].exists(_.name == "<lambda>0") shouldBe false
    }

    "create a method body for the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").block.astChildren.l match {
        case List(fallBack: Local, returnNode: Return) =>
          returnNode.code shouldBe "return lambdaInput.length() > 5 ? \"Long\" : fallback;"
          returnNode.astChildren.l match {
            case List(expr: Call) =>
              expr.methodFullName shouldBe Operators.conditional

            case result => fail(s"Expected return conditional, but got $result")
          }
          fallBack.name shouldBe "fallback"

        case result => fail(s"Expected lambda body with single return but got $result")
      }
    }

    "create a single ref edge from the lambdaInput identifier to the lambdaInput parameter" in {
      inside(cpg.typeDecl.name("Foo").method.name(".*lambda.*").ast.isIdentifier.name("lambdaInput").l) {
        case List(lambdaInputIdentifier) =>
          inside(lambdaInputIdentifier.refsTo.l) { case List(lambdaInputParameter: MethodParameterIn) =>
            lambdaInputParameter.name shouldBe "lambdaInput"
            lambdaInputIdentifier.method.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          }
      }
    }

    "create locals for captured identifiers in the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").local.sortBy(_.name) match {
        case Seq(fallbackLocal: Local) =>
          fallbackLocal.name shouldBe "fallback"
          fallbackLocal.code shouldBe "fallback"
          fallbackLocal.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single local for fallback but got $result")
      }
    }

    "create ref edges from the capture uses to the created locals" in {
      inside(cpg.typeDecl.name("Foo").method.name(".*lambda.*").local.l) { case List(fallbackLocal) =>
        fallbackLocal.name shouldBe "fallback"
        inside(fallbackLocal.referencingIdentifiers.l) { case List(fallbackUse) =>
          fallbackUse.name shouldBe "fallback"
          fallbackUse.typeFullName shouldBe "java.lang.String"
        }

      }
    }

    "create closure bindings for captured identifiers" in {
      cpg.closureBinding.l match {
        case List(fallbackClosureBinding) =>
          val fallbackLocal = cpg.method.name(".*lambda.*").local.name("fallback").head
          fallbackClosureBinding.closureBindingId shouldBe fallbackLocal.closureBindingId

          fallbackClosureBinding._refOut.l match {
            case List(capturedParam: MethodParameterIn) =>
              capturedParam.name shouldBe "fallback"
              capturedParam.method.fullName shouldBe "Foo.test1:void(java.lang.String,java.lang.String)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          fallbackClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.methodFullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected single METHOD_REF but got $result")
          }
        case result => fail(s"Expected 2 closure bindings for captured variables but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "<lambda>0"
          lambdaDecl.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("java.util.function.Function")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    "create bindings to implemented method" in {
      cpg.all.collectAll[Binding].nameExact("apply").sortBy(_.signature).toList match {
        case List(erasedBinding, binding) =>
          binding.methodFullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          binding.signature shouldBe "java.lang.String(java.lang.String)"
          binding.bindingTypeDecl.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"

          erasedBinding.methodFullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          erasedBinding.signature shouldBe "java.lang.Object(java.lang.Object)"
          erasedBinding.bindingTypeDecl.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"

        case result => fail(s"Expected two bindings to apply method but got $result")
      }
    }
  }

  "lambdas assigned to a variable an a vardecl" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public void test(String input, String fallback) {
        |    Function<String, String> mapper = lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
        |  }
        |}
        |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas reassigned to a variable" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public void test(String input, String fallback, Function<String, String> mapper) {
        |    mapper = lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
        |  }
        |}
        |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas returned from a function" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public Function<String, String> test(String input, String fallback) {
        |    return lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
        |  }
        |}
        |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Foo.<lambda>0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas capturing instance vars" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public String s;
        |
        |    public static void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    "create a 0th `this` parameter" in {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(thisParam, inputParam) =>
          thisParam.name shouldBe "this"
          thisParam.code shouldBe "this"
          thisParam.typeFullName shouldBe "Foo"
          thisParam.index shouldBe 0

          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.index shouldBe 1

        case result => fail(s"Expected two params for lambda method but got $result")
      }
    }
  }

  "lambdas calling instance methods" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static String s;
        |
        |    public void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    // TODO: Add 0th parameter logic
    "create a 0th `this` parameter" in {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(thisParam, inputParam) =>
          thisParam.name shouldBe "this"
          thisParam.code shouldBe "this"
          thisParam.typeFullName shouldBe "Foo"
          thisParam.order shouldBe 0
          thisParam.index shouldBe 0

          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.order shouldBe 1
          inputParam.index shouldBe 1

        case result => fail(s"Expected two params for lambda method but got $result")
      }
    }
  }

  "a lambda capturing a member" should {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |  public static String source() { return "hello"; }
        |  public static void sink(String s) { System.out.println(s); }
        |
        |  static void sinksSupplier(Supplier<String> supplier) {
        |    sink(supplier.get());
        |  }
        |
        |  private String value = source();
        |
        |  void test() {
        |    sinksSupplier(() -> value);
        |  }
        |}
        |""".stripMargin)

    "create a `this` local for the captured outer class" in {
      inside(cpg.method.name(".*lambda.*").local.l) { case List(thisLocal) =>
        thisLocal.name shouldBe "this"
        thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
      }
    }

    "create a closure binding referring to the captured this param" in {
      inside(cpg.closureBinding.l) { case List(thisClosureBinding) =>
        thisClosureBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        inside(thisClosureBinding._refOut.l) { case List(capturedParam: MethodParameterIn) =>
          capturedParam.name shouldBe "this"
          capturedParam.index shouldBe 0
          capturedParam.method.fullName shouldBe "Test.test:void()"
        }
      }
    }

    "represent the capture use as a field access on the captured this local" in {
      inside(cpg.method.name(".*lambda.*").ast.isReturn.astChildren.l) { case List(valueFieldAccess: Call) =>
        valueFieldAccess.name shouldBe Operators.fieldAccess

        inside(valueFieldAccess.argument.l) {
          case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
            thisIdentifier.name shouldBe "this"
            thisIdentifier.typeFullName shouldBe "Test"
            inside(thisIdentifier._refOut.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
              thisLocal.method.fullName.l shouldBe List("Test.<lambda>0:java.lang.String()")
            }

            valueFieldIdentifier.canonicalName shouldBe "value"
        }
      }
    }
  }

  "a lambda capturing a member in a nested class" should {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |  public static String source() { return "hello"; }
        |  public static void sink(String s) { System.out.println(s); }
        |
        |  static void sinksSupplier(Supplier<String> supplier) {
        |    sink(supplier.get());
        |  }
        |
        |  private String value = source();
        |
        |  void test() {
        |    class LocalClass {
        |      public void localClassMethod() {
        |        sinksSupplier(() -> value);
        |      }
        |    }
        |
        |    new LocalClass().localClassMethod();
        |  }
        |}
        |""".stripMargin)

    "have a `this` local for the captured outer class" in {
      inside(cpg.method.name(".*lambda.*").local.l) { case List(thisLocal) =>
        thisLocal.name shouldBe "this"
        thisLocal.typeFullName shouldBe "Test.test.LocalClass"
        thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
      }
    }

    "create a closure binding for the `this` local" in {
      inside(cpg.closureBinding.l) { case List(thisClosureBinding) =>
        thisClosureBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        inside(thisClosureBinding._refOut.l) { case List(capturedParam: MethodParameterIn) =>
          capturedParam.name shouldBe "this"
          capturedParam.index shouldBe 0
          capturedParam.method.fullName shouldBe "Test.test.LocalClass.localClassMethod:void()"
        }
      }
    }

    "represent the capture use as a field access via the outerClass field in the captured `this`" in {
      inside(cpg.method.name(".*lambda.*").ast.isReturn.astChildren.l) { case List(valueFieldAccess: Call) =>
        valueFieldAccess.name shouldBe Operators.fieldAccess

        inside(valueFieldAccess.argument.l) {
          case List(outerClassAccess: Call, valueFieldIdentifier: FieldIdentifier) =>
            outerClassAccess.name shouldBe Operators.fieldAccess

            inside(outerClassAccess.argument.l) {
              case List(thisIdentifier: Identifier, outerClassField: FieldIdentifier) =>
                thisIdentifier.name shouldBe "this"
                thisIdentifier.typeFullName shouldBe "Test.test.LocalClass"
                inside(thisIdentifier._refOut.l) { case List(thisLocal: Local) =>
                  thisLocal.name shouldBe "this"
                  thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
                  thisLocal.method.fullName.l shouldBe List("Test.test.LocalClass.<lambda>0:java.lang.String()")
                }

                outerClassField.canonicalName shouldBe "outerClass"
            }

            valueFieldIdentifier.canonicalName shouldBe "value"
        }
      }
    }
  }

  "a nested lambda capturing a parameter captured by a nested class" ignore {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |  public static String source() { return "hello"; }
        |  public static void sink(String s) { System.out.println(s); }
        |
        |  static void sinksSupplier(Supplier<String> supplier) {
        |    sink(supplier.get());
        |  }
        |
        |  void test(String value) {
        |    class LocalClass {
        |      public void localClassMethod() {
        |        sinksSupplier(() -> value);
        |      }
        |    }
        |
        |    new LocalClass().localClassMethod();
        |  }
        |}
        |""".stripMargin)

    "have a `this` local for the captured outer class" in {
      inside(cpg.method.name(".*lambda.*").local.l) { case List(thisLocal) =>
        thisLocal.name shouldBe "this"
        thisLocal.typeFullName shouldBe "Test.test.LocalClass"
        thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
      }
    }

    "create a closure binding for the `this` local" in {
      inside(cpg.closureBinding.l) { case List(thisClosureBinding) =>
        thisClosureBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        inside(thisClosureBinding._refOut.l) { case List(capturedParam: MethodParameterIn) =>
          capturedParam.name shouldBe "this"
          capturedParam.index shouldBe 0
          capturedParam.method.fullName shouldBe "Test.test.LocalClass.localClassMethod:void()"
        }
      }
    }

    "represent the field access as a field access on the captured variable field in the outer class" in {
      inside(cpg.method.name(".*lambda.*").ast.isReturn.astChildren.l) { case List(valueFieldAccess: Call) =>
        valueFieldAccess.name shouldBe Operators.fieldAccess

        inside(valueFieldAccess.argument.l) {
          case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
            thisIdentifier.name shouldBe "this"
            thisIdentifier.typeFullName shouldBe "Test.test.LocalClass"
            inside(thisIdentifier._refOut.l) { case List(thisLocal: Local) =>
              thisLocal.name shouldBe "this"
              thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
              thisLocal.method.fullName.l shouldBe List("Test.test.LocalClass.<lambda>0:java.lang.String()")
            }

            // When a local class captures a parameter, it's stored as a field in the local class
            // The field identifier should refer to the captured parameter field
            valueFieldIdentifier.canonicalName shouldBe "value"
        }
      }
    }
  }

  "lambdas using only static context" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static String s;
        |
        |    public static void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    "not create a 0th `this` parameter" in {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(inputParam) =>
          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.order shouldBe 1
          inputParam.index shouldBe 1

        case result => fail(s"Expected a single param for lambda method but got $result")
      }
    }
  }

  "lambdas with multiple statements in the body" should {
    val cpg = code("""
                     |import java.util.function.Function;
                     |
                     |public class Foo {
                     |    public static String s;
                     |
                     |    public static String f(String s) { return s;}
                     |
                     |    public Function<String, String> test() {
                     |        return input -> {
                     |          String concatenated = input + s;
                     |          return f(concatenated);
                     |        };
                     |    }
                     |}
                     |""".stripMargin)

    "have the correct method body with locals for captured variables" in {
      val nodes = cpg.method.name(".*lambda.*").body.astChildren.l
      inside(cpg.method.name(".*lambda.*").body.astChildren.l) {
        case List(thisLocal: Local, concatenated: Local, assignment: Call, ret: Return) =>
          thisLocal.name shouldBe "this"
          thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
          thisLocal.typeFullName shouldBe "Foo"

          concatenated.order shouldBe 2
          concatenated.name shouldBe "concatenated"
          concatenated.typeFullName shouldBe "java.lang.String"

          assignment.order shouldBe 3
          assignment.methodFullName shouldBe Operators.assignment

          ret.order shouldBe 4
          inside(ret.astChildren.l) { case List(call: Call) =>
            call.name shouldBe "f"
            call.methodFullName shouldBe "Foo.f:java.lang.String(java.lang.String)"
          }
      }
    }
  }

  "single-statement lambdas with no return values" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static void sink(String s) {};
        |
        |    public static Consumer<String> test() {
        |        return input -> sink(input);
        |    }
        |}
        |""".stripMargin)

    "have a method body block without a return statement" in {
      cpg.method.name(".*lambda.*").body.astChildren.l match {
        case List(call: Call) =>
          call.methodFullName shouldBe "Foo.sink:void(java.lang.String)"
          call.argument.l match {
            case List(identifier: Identifier) =>
              identifier.name shouldBe "input"
              identifier.typeFullName shouldBe "java.lang.String"
            case result => fail(s"Expected single String identifier a call arg but got $result")
          }
        case result => fail(s"Expected single call in body but got $result")
      }
    }
  }

  "lambdas with an Integer param type followed by a Float param type" should {
    val cpg = code("""
        |import java.util.function.BiConsumer;
        |class Foo {
        |  public static void sink(Float i, String f) {}
        |
        |  public static BiConsumer<Float, String> foo() {
        |    return (input1, input2) -> sink(input1, input2);
        |  }
        |}
        |""".stripMargin)

    "resolve calls in the body of the lambda" in {
      cpg.method.name(".*lambda.*").call.name("sink").l match {
        case sink :: Nil =>
          sink.methodFullName shouldBe "Foo.sink:void(java.lang.Float,java.lang.String)"
          sink.signature shouldBe "void(java.lang.Float,java.lang.String)"

        case result => fail(s"Expected single call to sink but got $result")
      }
    }
  }

  "lambdas implementing a user-defined functional interface" should {
    val cpg = code(
      """
        |public interface Foo<T, R> {
        |
        |    default String foo() {
        |        return "FOO";
        |    }
        |
        |    String baz(T input, R moreInput);
        |
        |    default T bar(T input) {
        |        return input;
        |    }
        |}
        |""".stripMargin,
      fileName = "Foo.java"
    ).moreCode("""
        |public class TestClass {
        |
        |    public static String foo(Integer x, Integer y, String z) { return z; }
        |
        |    public static Foo<Integer, Integer> test(String captured) {
        |        return (input, moreInput) -> foo(input, moreInput, captured);
        |    }
        |}
        |""".stripMargin)

    "create a method node for the lambda" in {
      cpg.typeDecl.name("TestClass").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"
          lambdaMethod.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Integer)"
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"
          lambdaMethod.parameter.l match {
            case List(input, moreInput) =>
              input.name shouldBe "input"
              input.code shouldBe "java.lang.Integer input"
              input.typeFullName shouldBe "java.lang.Integer"
              input.order shouldBe 1
              input.index shouldBe 1

              moreInput.name shouldBe "moreInput"
              moreInput.code shouldBe "java.lang.Integer moreInput"
              moreInput.typeFullName shouldBe "java.lang.Integer"
              moreInput.order shouldBe 2
              moreInput.index shouldBe 2

            case result => fail(s"Expected two lambda parameters but got $result")
          }

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "create the method body for the lambda" in {
      cpg.typeDecl.name("TestClass").method.name(".*lambda.*").body.astChildren.l match {
        case List(capturedLocal: Local, ret: Return) =>
          capturedLocal.name shouldBe "captured"
          capturedLocal.typeFullName shouldBe "java.lang.String"

          ret.order shouldBe 2
          ret.astChildren.size shouldBe 1
          ret.astChildren.collectAll[Call].size shouldBe 1
          val fooCall = ret.astChildren.collectAll[Call].head
          fooCall.name shouldBe "foo"
          fooCall.methodFullName shouldBe "TestClass.foo:java.lang.String(java.lang.Integer,java.lang.Integer,java.lang.String)"
          fooCall.typeFullName shouldBe "java.lang.String"
          fooCall.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Integer,java.lang.String)"
          fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

          fooCall.argument.l match {
            case List(input: Identifier, moreInput: Identifier, captured: Identifier) =>
              input.name shouldBe "input"
              input.typeFullName shouldBe "java.lang.Integer"

              moreInput.name shouldBe "moreInput"
              moreInput.typeFullName shouldBe "java.lang.Integer"

              captured.name shouldBe "captured"
              captured.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected three identifier arguments to foo but got $result")
          }

        case result => fail(s"Expected single captured local and return in method body but got $result")
      }
    }

    "not create a binding for the lambda method" in {
      cpg.all.collectAll[Binding].exists(_.name == "<lambda>0") shouldBe false
    }

    "create closure bindings for captured identifiers" in {
      cpg.closureBinding.l match {
        case List(capturedClosureBinding) =>
          capturedClosureBinding.label shouldBe "CLOSURE_BINDING"

          val capturedLocal = cpg.method.name(".*lambda.*").local.name("captured").head
          capturedClosureBinding.closureBindingId shouldBe capturedLocal.closureBindingId

          capturedClosureBinding._refOut.l match {
            case List(capturedParam: MethodParameterIn) =>
              capturedParam.name shouldBe "captured"
              capturedParam.method.fullName shouldBe "TestClass.test:Foo(java.lang.String)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          capturedClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.methodFullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"
            case result => fail(s"Expected single out METHOD_REF but got $result")
          }

        case result => fail(s"Expected 2 closure bindings for captured variables but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "<lambda>0"
          lambdaDecl.fullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("Foo")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    // TODO: Fix typeDecl for interfaceBinding
    "create bindings to implemented method" in {
      cpg.all.collectAll[Binding].nameExact("baz").sortBy(_.methodFullName).toList match {
        case List(interfaceBinding, binding, erasedBinding) =>
          interfaceBinding.methodFullName shouldBe "Foo.baz:java.lang.String(java.lang.Object,java.lang.Object)"
          interfaceBinding.signature shouldBe "java.lang.String(java.lang.Object,java.lang.Object)"
          interfaceBinding.bindingTypeDecl.fullName shouldBe "Foo"

          binding.methodFullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"
          binding.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Integer)"
          binding.bindingTypeDecl.fullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"

          erasedBinding.methodFullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"
          erasedBinding.signature shouldBe "java.lang.String(java.lang.Object,java.lang.Object)"
          erasedBinding.bindingTypeDecl.fullName shouldBe "TestClass.<lambda>0:java.lang.String(java.lang.Integer,java.lang.Integer)"

        case result => fail(s"Expected three bindings to baz method but got $result")
      }
    }
  }

  // This is an example of a case where all the type info we need to figure out that the lambda implements
  // `Function<String, String>`, but from JavaParser and the current expectedType propagation we only get
  // `java.util.Function<Object, Object>`. Ideally we'd want all occurrences of `java.lang.Object` to be
  // `java.lang.String` instead.
  "a lambda implementing a mapper in stream" should {
    val cpg = code("""
        |import java.util.List;
        |import java.util.stream.Collectors;
        |
        |public class Foo {
        |  public List<String> method(List<String> list) {
        |    return list.stream().map(string -> string).collect(Collectors.toList());
        |  }
        |}
        |""".stripMargin)

    "create the correct code string for call chains involving lambdas" in {
      cpg.call.name("collect").l match {
        case List(collectCall) =>
          collectCall.code shouldBe "list.stream().map(<lambda>).collect(Collectors.toList())"

        case result => fail(s"Expected single call to collect but got $result")
      }
    }

    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Foo.<lambda>0:java.lang.Object(java.lang.Object)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "string"
              lambdaInput.typeFullName shouldBe "java.lang.Object"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.Object"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "not create a binding for the lambda method" in {
      cpg.all.collectAll[Binding].exists(_.name == "<lambda>0") shouldBe false
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "<lambda>0"
          lambdaDecl.fullName shouldBe "Foo.<lambda>0:java.lang.Object(java.lang.Object)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("java.util.function.Function")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    "create bindings to implemented method" in {
      cpg.all.collectAll[Binding].nameExact("apply").sortBy(_.signature).toList match {
        case List(binding) =>
          binding.methodFullName shouldBe "Foo.<lambda>0:java.lang.Object(java.lang.Object)"
          binding.signature shouldBe "java.lang.Object(java.lang.Object)"
          binding.bindingTypeDecl.fullName shouldBe "Foo.<lambda>0:java.lang.Object(java.lang.Object)"

        case result => fail(s"Expected single binding to apply method but got $result")
      }
    }
  }

  "lambda capturing local variables" should {

    val cpg = code("""
        |public class Foo {
        |
        | void sink2(Object o) {}
        |
        | public void foo(Object arg) {
        |       String myValue = "abc";
        |       List<String> userPayload = new ArrayList<>();
        |       List<String> userNamesList = userPayload.stream.map(item -> {
        |         sink2(myValue);
        |         return item + myValue;
        |       });
        |       sink1(userNamesList);
        |       return;
        | }
        |
        |}
        |""".stripMargin)

    "be captured precisely" in {
      inside(cpg.closureBinding.sortBy(_.closureBindingId).l) { case List(myValue, thisClosureBinding) =>
        myValue.closureBindingId shouldBe Some("Test0.java:<lambda>0:myValue")
        myValue._localViaRefOut.get.name shouldBe "myValue"
        myValue._captureIn.collectFirst { case x: MethodRef =>
          x.methodFullName
        }.head shouldBe "Foo.<lambda>0:<unresolvedSignature>(1)"

        thisClosureBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        inside(thisClosureBinding._refOut.l) { case List(thisParam: MethodParameterIn) =>
          thisParam.name shouldBe "this"
          thisParam.method.fullName shouldBe "Foo.foo:void(java.lang.Object)"
        }
      }
    }

    "correctly represent implicit captured this receiver" in {
      inside(cpg.call.name("sink2").receiver.l) { case List(thisIdentifier: Identifier) =>
        thisIdentifier.name shouldBe "this"
        thisIdentifier.typeFullName shouldBe "Foo"

        inside(thisIdentifier.refsTo.l) { case List(thisLocal: Local) =>
          thisLocal.name shouldBe "this"
          thisLocal.typeFullName shouldBe "Foo"
          thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        }
      }
    }
  }

  "object creation expressions in lambdas" should {
    val cpg = code("""
        |import java.util.ArrayList;
        |
        |public class Foo {
        |  public Integer method(ArrayList<Integer> list, Integer aaa) {
        |    ArrayList<Integer> mappedList = list.stream().map(integer -> {
        |      ArrayList<Integer> nestedList = new ArrayList<>();
        |    });
        |
        |    return 0;
        |  }
        |}
        |""".stripMargin)

    "not create identifiers without parents" in {
      cpg.identifier.name("nestedList").astParent.nonEmpty shouldBe true
    }

    "have init args created correctly" in {
      cpg.call.nameExact("<init>").count(_.argument.isEmpty) shouldBe 0
    }
  }

  "variable captures in nested lambdas" should {
    val cpg = code("""package io.shiftleft.testcode.dataflow;
                     |
                     |import java.util.ArrayList;
                     |import java.util.List;
                     |import java.util.stream.Collectors;
                     |
                     |public class DataFlow27 {
                     |  public Integer method(Integer aaa) {
                     |    List<Integer> list = new ArrayList<>();
                     |    list.add(1);
                     |
                     |    List<Integer> mappedList = list.stream().map(integer -> {
                     |      List<Integer> nestedList = new ArrayList<>();
                     |      nestedList.add(1);
                     |
                     |      List<Integer> nestedMappedList =
                     |          nestedList.stream().map(nestedInteger -> nestedInteger + aaa).collect(Collectors.toList());
                     |      return nestedMappedList.get(0);
                     |    }).collect(Collectors.toList());
                     |    Integer ret = mappedList.get(0);
                     |    return ret;
                     |  }
                     |}
                     |""".stripMargin)

    "have a corresponding local in the outer lambda" in {
      val locals = cpg.method.fullName(".*lambda.*0.*").local.l
      inside(cpg.method.fullName(".*lambda.*0.*").local.sortBy(_.name).l) {
        case List(aaaLocal, nestedList, nestedMappedListLocal) =>
          aaaLocal.name shouldBe "aaa"
          aaaLocal.typeFullName shouldBe "java.lang.Integer"
          aaaLocal.closureBindingId.l shouldBe List("Test0.java:<lambda>0:aaa")

          nestedList.name shouldBe "nestedList"

          nestedMappedListLocal.name shouldBe "nestedMappedList"
      }
    }

    "have a corresponding local in the nested lambda referenced by the identifier" in {
      inside(cpg.method.fullName(".*lambda.*1.*").local.l) { case List(aaaLocal) =>
        aaaLocal.name shouldBe "aaa"
        aaaLocal.typeFullName shouldBe "java.lang.Integer"
        aaaLocal.closureBindingId.l shouldBe List("Test0.java:<lambda>1:aaa")

        inside(aaaLocal.referencingIdentifiers.l) { case List(aaaIdentifier) =>
          aaaIdentifier.name shouldBe "aaa"
          aaaIdentifier.typeFullName shouldBe "java.lang.Integer"
          aaaIdentifier.refsTo.l shouldBe List(aaaLocal)
        }
      }
    }

    "create the correct closure binding for the captured variable" in {
      inside(cpg.closureBinding.sortBy(_.closureBindingId).l) { case List(lambda0Binding, lambda1Binding) =>
        lambda0Binding.closureBindingId shouldBe Some("Test0.java:<lambda>0:aaa")
        lambda0Binding._refOut.l shouldBe cpg.method("method").parameter.name("aaa").l
        inside(lambda0Binding.captureIn.l) { case List(methodRef: MethodRef) =>
          methodRef.methodFullName shouldBe "io.shiftleft.testcode.dataflow.DataFlow27.<lambda>0:java.lang.Object(java.lang.Object)"
        }

        lambda1Binding.closureBindingId shouldBe Some("Test0.java:<lambda>1:aaa")
        lambda1Binding._refOut.l shouldBe cpg.method.fullName(".*lambda.*0.*").local.name("aaa").l
        inside(lambda1Binding.captureIn.l) { case List(methodRef: MethodRef) =>
          methodRef.methodFullName shouldBe "io.shiftleft.testcode.dataflow.DataFlow27.<lambda>1:java.lang.Object(java.lang.Object)"
        }
      }
    }

  }

  "calls on captured variables in lambdas contained in anonymous classes" should {
    val cpg = code("""
        |
        |public class Foo {
        |
        |    public static void sink(String s) {};
        |
        |    public static Object test(Bar captured) {
        |      Visitor v = new Visitor() {
        |        public void visit(Visited visited) {
        |          visited.getList().forEach(lambdaParam -> captured.remove(lambdaParam));
        |        }
        |      };
        |    }
        |}
        |""".stripMargin)

    // TODO: captured should be a `this.captured` fieldAccess where `this` is the local with a closure binding referring
    //  to the `this` param of visit
    "have the correct receiver ast" ignore {
      val call = cpg.call.name("remove").l
      inside(cpg.call.name("remove").receiver.l) { case List(fieldAccessCall: Call) =>
        fieldAccessCall.name shouldBe Operators.fieldAccess

        inside(fieldAccessCall.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo.test.Visitor$0"

          fieldIdentifier.canonicalName shouldBe "captured"

          fieldAccessCall.typeFullName shouldBe "<unresolvedNamespace>.Bar"
        }
      }
    }
  }

  "lambdas capturing parameters" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |  public String capturedField;
        |
        |  public void foo() {
        |    Consumer<String> consumer = lambdaParam -> System.out.println(capturedField);
        |  }
        |}
        |""".stripMargin)

    "represent the captured field as a field access" in {
      inside(cpg.method.name(".*lambda.*").call.name("println").argument.l) { case List(_, fieldAccessCall: Call) =>
        fieldAccessCall.name shouldBe Operators.fieldAccess

        inside(fieldAccessCall.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"

          fieldIdentifier.canonicalName shouldBe "capturedField"
        }
      }
    }

    "have a captured local for the enclosing class" in {
      // There should be an `outerClass` local which captures the outer method `this`.
      inside(cpg.method.name(".*lambda.*").local.name("this").l) { case List(thisLocal) =>
        thisLocal.typeFullName shouldBe "Foo"
        thisLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:this")
        inside(thisLocal.referencingIdentifiers.l) { case List(thisIdentifier) =>
          thisIdentifier.name shouldBe "this"
          thisIdentifier.astParent.code shouldBe "this.capturedField"
        }
      }
    }
  }

  "lambdas with unused `captured` variables in scope" should {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |
        |  public void test(String value) {
        |    Supplier<String> s = () -> "test";
        |  }
        |}
        |""".stripMargin)

    "not have a local corresponding to the value parameter in the lambda" in {
      cpg.method.name(".*lambda.*").local.l shouldBe Nil
    }
  }

  "lambdas with a capture use in a switch selector" should {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |
        |  public void test(Integer value) {
        |    Supplier<String> s = () -> {
        |      switch (value) {
        |        case 1 -> "one";
        |        case 2 -> "two";
        |        default -> "other";
        |      };
        |      return "";
        |    };
        |  }
        |}
        |""".stripMargin)

    "have a local for the captured variable" in {
      inside(cpg.method.name(".*lambda.*").local.l) { case List(valueLocal) =>
        valueLocal.name shouldBe "value"
        valueLocal.typeFullName shouldBe "java.lang.Integer"
        valueLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:value")
      }
    }

    "have a closure binding for the captured variable" in {
      inside(cpg.closureBinding.l) { case List(valueBinding) =>
        valueBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:value")
        inside(valueBinding._refOut.l) { case List(valueParam: MethodParameterIn) =>
          valueParam.name shouldBe "value"
          valueParam.method.fullName shouldBe "Test.test:void(java.lang.Integer)"
        }
      }
    }

    "have a ref edge from the switch selector identifier to the local" in {
      inside(cpg.method.name(".*lambda.*").ast.isIdentifier.name("value").l) { case List(valueIdentifier) =>
        valueIdentifier.name shouldBe "value"
        inside(valueIdentifier.refsTo.l) { case List(valueLocal: Local) =>
          valueLocal.name shouldBe "value"
          valueLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:value")
        }
      }
    }
  }

  "lambdas with a capture use in a switch entry" should {
    val cpg = code("""
        |import java.util.function.Supplier;
        |
        |class Test {
        |
        |  public void test(String prefix) {
        |    Supplier<String> s = () -> {
        |      int x = 1;
        |      switch (x) {
        |        case 1 -> prefix + "one";
        |        case 2 -> "two";
        |        default -> "other";
        |      };
        |      return "";
        |    };
        |  }
        |}
        |""".stripMargin)

    "have a local for the captured variable" in {
      inside(cpg.method.name(".*lambda.*").local.name("prefix").l) { case List(prefixLocal) =>
        prefixLocal.name shouldBe "prefix"
        prefixLocal.typeFullName shouldBe "java.lang.String"
        prefixLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:prefix")
      }
    }

    "have a closure binding for the captured variable" in {
      inside(cpg.closureBinding.l) { case List(prefixBinding) =>
        prefixBinding.closureBindingId shouldBe Some("Test0.java:<lambda>0:prefix")
        inside(prefixBinding._refOut.l) { case List(prefixParam: MethodParameterIn) =>
          prefixParam.name shouldBe "prefix"
          prefixParam.method.fullName shouldBe "Test.test:void(java.lang.String)"
        }
      }
    }

    "have a ref edge from the identifier in the switch entry to the local" in {
      inside(cpg.method.name(".*lambda.*").ast.isIdentifier.name("prefix").l) { case List(prefixIdentifier) =>
        prefixIdentifier.name shouldBe "prefix"
        inside(prefixIdentifier.refsTo.l) { case List(prefixLocal: Local) =>
          prefixLocal.name shouldBe "prefix"
          prefixLocal.closureBindingId shouldBe Some("Test0.java:<lambda>0:prefix")
        }
      }
    }
  }
}
