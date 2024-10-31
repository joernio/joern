package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.semanticcpg.language.*
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{
  Binding,
  Block,
  Call,
  FieldIdentifier,
  Identifier,
  TypeDecl,
  TypeRef
}
import io.shiftleft.codepropertygraph.generated.Operators

class AnonymousClassTests extends JavaSrcCode2CpgFixture {

  "simple anonymous classes extending interfaces in method bodies" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  void bar();
        |}
        |
        |class Foo {
        |  static void sink(String s) {}
        |
        |  void foo() {
        |    Bar b = new Bar() {
        |      public void bar() {
        |        sink("BAR");
        |      }
        |    };
        |
        |    b.bar();
        |  }
        |}
        """.stripMargin)

    def anonymousDeclQuery = cpg.typeDecl.name("Foo").ast.collectAll[TypeDecl].nameExact("Bar$0")

    "have a binding for the explicit method" in {
      inside(cpg.all.collectAll[Binding].name("bar").sortBy(_.methodFullName.length).l) {
        case List(interfaceBinding, anonBinding) =>
          interfaceBinding.methodFullName shouldBe "foo.Bar.bar:void()"
          interfaceBinding.bindingTypeDecl.fullName shouldBe "foo.Bar"

          anonBinding.methodFullName shouldBe "foo.Foo.foo.Bar$0.bar:void()"
          anonBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.foo.Bar$0"
      }
    }

    "have a type declaration for the anonymous class" in {
      cpg.typeDecl.name("Bar.*").name.toSet shouldBe Set("Bar", "Bar$0")
      cpg.typeDecl.name("Bar.*").fullName.toSet shouldBe Set("foo.Bar", "foo.Foo.foo.Bar$0")
    }

    "have the inheritsFromTypeFullName property correctly set" in {
      anonymousDeclQuery.inheritsFromTypeFullName.toList shouldBe List("foo.Bar")
    }

    "define a default constructor with parameters and initializers for captures" in {
      inside(anonymousDeclQuery.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.foo.Bar$0.<init>:void()"

        inside(constructor.parameter.l) { case List(thisParam, outerClassParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.foo.Bar$0"
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.foo.Bar$0")

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "foo.Foo"
        }

        inside(constructor.body.astChildren.l) { case List(outerClassAssign: Call) =>
          outerClassAssign.name shouldBe Operators.assignment
          outerClassAssign.code shouldBe "this.outerClass = outerClass"
        }
      }
    }

    "have the correct fullName" in {
      anonymousDeclQuery.fullName.l shouldBe List("foo.Foo.foo.Bar$0")
    }

    "be constructed correctly where the class is defined" in {
      inside(cpg.method.name("foo").call.nameExact("<init>").l) { case List(initCall) =>
        initCall.methodFullName shouldBe "foo.Foo.foo.Bar$0.<init>:void()"
        inside(initCall.argument.l) { case List(thisArgument: Identifier, outerClassArgument: Identifier) =>
          thisArgument.name shouldBe "b"
          thisArgument.typeFullName shouldBe "foo.Bar"

          outerClassArgument.name shouldBe "this"
          outerClassArgument.refOut.l shouldBe cpg.method.name("foo").parameter.name("this").l
        }
      }
    }
  }

  "simple anonymous classes extending classes in fields" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  void bar();
        |}
        |
        |class Foo {
        |  Bar b = new Bar() {
        |    void bar() {
        |      sink("BAR");
        |    }
        |  };
        |
        |  void foo() {
        |    b.bar();
        |  }
        |}
        |""".stripMargin)

    def anonymousDeclQuery = cpg.typeDecl.name("Foo").ast.collectAll[TypeDecl].nameExact("Bar$0")

    "the correct fullName" in {
      anonymousDeclQuery.fullName.toList shouldBe List("foo.Foo.b.Bar$0")
    }

    "have the inheritsFromTypeFullName property correctly set" in {
      anonymousDeclQuery.inheritsFromTypeFullName.toSet shouldBe Set("foo.Bar")
    }

    "be constructed correctly in the enclosing class's init" in {
      inside(cpg.typeDecl.name("Foo").method.nameExact("<init>").call.nameExact("<init>").l) { case List(initCall) =>
        initCall.methodFullName shouldBe "foo.Foo.b.Bar$0.<init>:void()"
        inside(initCall.argument.l) { case List(thisArgument: Call, outerClassArgument: Identifier) =>
          thisArgument.name shouldBe Operators.fieldAccess
          thisArgument.typeFullName shouldBe "foo.Bar"
          inside(thisArgument.argument.l) { case List(thisNode: Identifier, bMember: FieldIdentifier) =>
            thisNode.name shouldBe "this"
            thisNode.typeFullName shouldBe "foo.Foo"

            bMember.canonicalName shouldBe "b"
          }

          outerClassArgument.name shouldBe "this"
          outerClassArgument.refOut.l shouldBe cpg.typeDecl
            .name("Foo")
            .method
            .nameExact("<init>")
            .parameter
            .name("this")
            .l
        }
      }
    }
  }

  "simple anonymous classes extending classes in static fields" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  void bar();
        |}
        |
        |class Foo {
        |  static Bar b = new Bar() {
        |    public void bar() {
        |      sink("BAR");
        |    }
        |  };
        |
        |  void foo() {
        |    b.bar();
        |  }
        |}
        |""".stripMargin)

    def anonymousDeclQuery = cpg.typeDecl.name("Foo").ast.collectAll[TypeDecl].nameExact("Bar$0")

    "be constructed correctly in the enclosing class's clinit" in {
      inside(cpg.typeDecl.name("Foo").method.nameExact("<clinit>").call.nameExact("<init>").l) { case List(initCall) =>
        initCall.methodFullName shouldBe "foo.Foo.b.Bar$0.<init>:void()"
        inside(initCall.argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.typeFullName shouldBe "foo.Bar"

          inside(fieldAccess.argument.l) { case List(typeRef: TypeRef, bField: FieldIdentifier) =>
            typeRef.typeFullName shouldBe "foo.Foo"
            bField.canonicalName shouldBe "b"
          }
        }
      }
    }
  }

  "simple anonymous classes extending classes for enum entries" should {
    val cpg = code("""
        |package foo;
        |
        |enum Foo {
        |  ENTRY(42) {
        |    @Override
        |    int getValue() {
        |      return value + 7;
        |    }
        |  };
        |
        |  protected int value;
        |
        |  int getValue() {
        |    return value;
        |  }
        |
        |  Foo(int value) {
        |    this.value = value;
        |  }
        |}
        |""".stripMargin)
    "be constructed correctly in the enclosing class's clinit" in {
      pendingUntilFixed {
        inside(cpg.typeDecl.name("Foo").method.nameExact("<clinit>").call.nameExact("<init>").l) {
          case List(initCall) =>
            initCall.methodFullName shouldBe "foo.Foo$0.<init>:void()"
            inside(initCall.argument.l) { case List(thisArgument: Identifier) =>
              thisArgument.name shouldBe "ENTRY"
              thisArgument.typeFullName shouldBe "foo.Foo$0"
            }
        }
      }
    }
  }

  "anonymous classes extending non-trivial classes" should {
    val cpg = code("""
        |package foo;
        |
        |abstract class Bar {
        |  int barMember = 0;
        |  void bar();
        |  void sink(int input) {}
        |}
        |
        |class Foo {
        |  static Bar b = new Bar() {
        |    public void bar() {
        |      sink(barMember);
        |    }
        |  };
        |
        |  void foo() {
        |    b.bar();
        |  }
        |}
        """.stripMargin)

    "be able to access members and methods from the superclass" in {
      inside(cpg.method.name("bar").call.name("sink").l) { case List(sinkCall) =>
        sinkCall.methodFullName shouldBe "foo.Foo.b.Bar$0.sink:void(int)"

        inside(sinkCall.argument.l) { case List(thisArgument: Identifier, memberAccess: Call) =>
          thisArgument.name shouldBe "this"
          thisArgument.typeFullName shouldBe "foo.Foo.b.Bar$0"

          memberAccess.code shouldBe "this.barMember"
        }
      }
    }
  }

  "multiple anonymous classes in the same method" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  void bar();
        |}
        |
        |class Foo {
        |  void foo() {
        |    Bar a = new Bar() {
        |      void bar() {
        |        sink("A");
        |      }
        |    };
        |    Bar b = new Bar() {
        |      void bar() {
        |        sink("B");
        |      }
        |    };
        |
        |    a.bar();
        |    b.bar();
        |  }
        |}
        """.stripMargin)

    "have different names" in {
      cpg.typeDecl.name("Bar.*").fullName.toSet shouldBe Set("foo.Bar", "foo.Foo.foo.Bar$0", "foo.Foo.foo.Bar$1")
    }

    "have the correct names matched to the correct constructors" in {
      cpg.method.name("foo").call.nameExact("<init>").methodFullName.toList shouldBe List(
        "foo.Foo.foo.Bar$0.<init>:void()",
        "foo.Foo.foo.Bar$1.<init>:void()"
      )
    }

    "have the correct types assigned to the bars" in {
      cpg.method
        .name("foo")
        .call
        .nameExact(Operators.assignment)
        .flatMap(call => call.argument.collectFirst { case call: Call => call })
        .typeFullName
        .toList shouldBe List("foo.Foo.foo.Bar$0", "foo.Foo.foo.Bar$1")
    }
  }

  // TODO: Probably ignore this case
  "anonymous classes that inherit non-trivial constructors from a local class with captures" ignore {
    val cpg = code("""
        |package foo;
        |
        |class Bar {
        |  int barMember;
        |  public Bar(int barParam) {
        |    barMember = barParam;
        |  }
        |}
        |
        |class OuterClass {
        |  void outerMethod(int outerParam) {
        |    class InnerClass {
        |      void innerMethod(int innerParam) {
        |        Bar b = new Bar(innerParam) {
        |          int bar() {
        |            return barMember + innerParam + outerParam;
        |          }
        |        }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin)
    "have the correct constructor parameters" in {
      ???
    }

    "initialize captures in the constructor" in {
      ???
    }

    "call the super constructor with the correct arguments" in {
      ???
    }
  }
}
