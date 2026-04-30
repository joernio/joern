package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal, Member}
import io.shiftleft.semanticcpg.language.*

class NewMemberTests extends JavaSrcCode2CpgFixture {
  "locals shadowing members" should {
    val cpg = code("""
                     |public class Foo {
                     |  Integer value = 12;
                     |
                     |  static void foo() {
                     |    String value = "Hello";
                     |    value.trim();
                     |  }
                     |}
                     |""".stripMargin)

    "use the local type for calls" in {
      cpg.call.name("trim").methodFullName.toList shouldBe List("java.lang.String.trim:java.lang.String()")
    }
  }

  "parameters shadowing members" should {
    val cpg = code("""
                     |public class Foo {
                     |  Integer value = 12;
                     |
                     |  static void foo(String value) {
                     |    value.trim();
                     |  }
                     |}
                     |""".stripMargin)

    "use the local type for calls" in {
      cpg.call.name("trim").methodFullName.toList shouldBe List("java.lang.String.trim:java.lang.String()")
    }
  }

  "members with anonymous classes" should {
    val cpg = code("""
        |class Foo {
        |  Foo x = new Foo() {
        |    @Override
        |    void foo() {}
        |  };
        |
        |  void foo() {}
        |}""".stripMargin)

    "not result in subtrees to the member node" in {
      def typeDecl = cpg.typeDecl.nameExact("Foo")
      typeDecl.size shouldBe 1

      typeDecl.member.size shouldBe 1
      typeDecl.member.name("x").astChildren.size shouldBe 0
    }

    "contain a class declaration for the anonymous class" in {
      inside(cpg.typeDecl.nameExact("Foo$0").l) { case List(anonDecl) =>
        anonDecl.fullName shouldBe "Foo.x.Foo$0"
        anonDecl.method.name.toSet shouldBe Set("<init>", "foo")
      }
    }
  }

  "member with generic class" should {
    val cpg = code("""
        |import org.apache.kafka.clients.consumer.Consumer;
        |public class CountryPopulationConsumer {
        |
        | private Consumer<String, Integer> consumer;
        |
        |  void foo() {
        |   consumer.poll(1000);
        |  }
        |}""".stripMargin)

    "have a resolved typeFullName" in {
      cpg.member
        .name("consumer")
        .typeFullName
        .head shouldBe "org.apache.kafka.clients.consumer.Consumer"
    }

    "have a resolved package name in methodFullName" in {
      cpg
        .call("poll")
        .methodFullName
        .head
        .split(":")
        .head shouldBe "org.apache.kafka.clients.consumer.Consumer.poll"
    }
  }

  "enum entries with anonymous classes should not result in subtrees to the member node" in {
    val cpg = code("""
        |enum Foo {
        |  X(12) {
        |    @Override
        |    void foo() {}
        |  };
        |
        |  private Foo(int x) {}
        |
        |  void foo() {}
        |}""".stripMargin)
    cpg.member.size shouldBe 1
    cpg.member.name("x").astChildren.size shouldBe 0
  }

  "non-static member initializers" should {
    "only be added once per constructor" in {
      val cpg = code("""
                      |class Foo {
                      |  int x = 1;
                      |
                      |  public Foo() {}
                      |
                      |  public Foo(int y) {
                      |    this.x = y;
                      |  }
                      |}""".stripMargin)

      cpg.method
        .fullNameExact(s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()")
        .assignment
        .astChildren
        .size shouldBe 2
    }
    "be added to the default constructor in classes with no constructor" in {
      val cpg = code("""
             |class Foo {
             |    int x = 1;
             |}""".stripMargin)

      val constructor = inside(cpg.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case constructor :: Nil => constructor
      }

      constructor.fullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
      val xAssign = inside(constructor.body.astChildren.l) { case List(xAssign: Call) =>
        xAssign
      }

      xAssign.name shouldBe Operators.assignment
      xAssign.methodFullName shouldBe Operators.assignment

      inside(xAssign.argument.l) { case List(fieldAccess: Call, value: Literal) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "x"
        }

        value.code shouldBe "1"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }
    }

    "be added to an empty constructor" in {
      val cpg = code("""
             |class Foo {
             |    int x = 1;
             |    public Foo() {}
             |}""".stripMargin)
      val constructor = inside(cpg.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case constructor :: Nil => constructor
      }

      constructor.fullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
      val xAssign = inside(constructor.body.astChildren.l) { case List(xAssign: Call) =>
        xAssign
      }

      xAssign.name shouldBe Operators.assignment
      xAssign.methodFullName shouldBe Operators.assignment

      inside(xAssign.argument.l) { case List(fieldAccess: Call, value: Literal) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "x"
        }

        value.code shouldBe "1"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }
    }

    "be added to a not-empty constructor without an explicit constructor invocation" in {
      val cpg = code("""
			 |class Foo {
			 |    int x = 1;
			 |    int y;
			 |
			 |    public Foo(int y) {
			 |      this.y = y;
			 |    }
			 |}""".stripMargin)

      val constructor = inside(cpg.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case constructor :: Nil => constructor
      }

      constructor.fullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
      val (xAssign, yAssign) = inside(constructor.body.astChildren.l) { case List(xAssign: Call, yAssign: Call) =>
        (xAssign, yAssign)
      }

      xAssign.name shouldBe Operators.assignment
      xAssign.methodFullName shouldBe Operators.assignment

      inside(xAssign.argument.l) { case List(fieldAccess: Call, value: Literal) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "x"

        }

        value.code shouldBe "1"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }

      yAssign.name shouldBe Operators.assignment
      yAssign.methodFullName shouldBe Operators.assignment

      inside(yAssign.argument.l) { case List(fieldAccess: Call, value: Identifier) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "y"
        }

        value.name shouldBe "y"
        value.code shouldBe "y"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }
    }

    "not be added to a constructor containing an explicit constructor invocation" in {
      val cpg = code("""
			 |class Foo {
			 |    int x = 1;
			 |    int y;
			 |
			 |    public Foo(int y) {
			 |        this.y = y;
			 |    }
			 |
			 |    public Foo() {
			 |        this(42);
			 |    }
			 |}""".stripMargin)

      val constructorWithParam =
        inside(cpg.method.fullNameExact(s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)").l) {
          case constructor :: Nil => constructor
        }

      val (xAssign, yAssign) = inside(constructorWithParam.body.astChildren.l) {
        case List(xAssign: Call, yAssign: Call) => (xAssign, yAssign)
      }

      xAssign.name shouldBe Operators.assignment
      xAssign.methodFullName shouldBe Operators.assignment

      inside(xAssign.argument.l) { case List(fieldAccess: Call, value: Literal) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "x"
        }

        value.code shouldBe "1"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }

      yAssign.name shouldBe Operators.assignment
      yAssign.methodFullName shouldBe Operators.assignment

      inside(yAssign.argument.l) { case List(fieldAccess: Call, value: Identifier) =>
        fieldAccess.name shouldBe Operators.fieldAccess
        inside(fieldAccess.argument.l) { case List(identifier: Identifier, fieldIdentifier: FieldIdentifier) =>
          identifier.name shouldBe "this"
          identifier.typeFullName shouldBe "Foo"
          fieldIdentifier.canonicalName shouldBe "y"
        }

        value.name shouldBe "y"
        value.code shouldBe "y"
        value.typeFullName shouldBe "int"
        value.argumentIndex shouldBe 2
      }

      val ctorWithExplInvocation =
        cpg.method.fullNameExact(s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()")
      inside(ctorWithExplInvocation.body.astChildren.l) { case List(explConsInvocation: Call) =>
        explConsInvocation.methodFullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
      }
    }
  }
}

class MemberTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |class Foo {
      |  int x;
      |}
      |
      |class Bar {
      |    int nonStatic;
      |
      |    static int isStatic = 0;
      |    static int isAlsoStatic;
      |
      |    static Bar isStaticObject = new Bar();
      |
      |    Bar anotherNonStatic = new Bar();
      |
      |    static {
      |        isAlsoStatic = 1;
      |    }
      |
      |    static {
      |        System.out.println("Second static block");
      |    }
      |}
      |""".stripMargin)

  private def hasStaticMod(member: Member): Boolean = {
    member.modifier.exists(_.modifierType == ModifierTypes.STATIC)
  }

  "should contain MEMBER node with correct properties" in {
    val List(x) = cpg.member("x").l
    x.name shouldBe "x"
    x.code shouldBe "int x"
    x.typeFullName shouldBe "int"
    x.order shouldBe 1
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val List(x) = cpg.member("x").typeDecl.l
    x.name shouldBe "Foo"
  }

  "should not create a <clinit> method for classes without static members" in {
    cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).size shouldBe 0
  }

  "should create correct static and non-static members for class with both" in {
    inside(cpg.typeDecl.name("Bar").member.l) {
      case List(nonStatic, isStatic, isAlsoStatic, isStaticObject, anotherNonStatic) =>
        nonStatic.name shouldBe "nonStatic"
        nonStatic.typeFullName shouldBe "int"
        hasStaticMod(nonStatic) shouldBe false

        isStatic.name shouldBe "isStatic"
        isStatic.typeFullName shouldBe "int"
        hasStaticMod(isStatic) shouldBe true

        isAlsoStatic.name shouldBe "isAlsoStatic"
        isAlsoStatic.typeFullName shouldBe "int"
        hasStaticMod(isAlsoStatic) shouldBe true

        isStaticObject.name shouldBe "isStaticObject"
        isStaticObject.typeFullName shouldBe "Bar"
        hasStaticMod(isStaticObject) shouldBe true

        anotherNonStatic.name shouldBe "anotherNonStatic"
        anotherNonStatic.typeFullName shouldBe "Bar"
        hasStaticMod(anotherNonStatic) shouldBe false
    }
  }

  "should create a single <clinit> method for classes with static members" in {
    pendingUntilFixed {

      cpg.typeDecl.nameExact("Bar").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).size shouldBe 1

      val clinit = cpg.typeDecl.nameExact("Bar").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).head
      clinit.signature shouldBe "void()"
      clinit.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.StaticInitMethodName}:void()"

      inside(clinit.body.astChildren.l) {
        case List(
              isStaticInit: Call,
              isStaticObjectAssign: Call,
              isStaticObjectInit: Call,
              isAlsoStaticInit: Call,
              printStmt: Call
            ) =>
          isStaticInit.methodFullName shouldBe Operators.assignment
          isStaticInit.argument.size shouldBe 2
          inside(isStaticInit.argument.headOption) { case Some(fieldAccess: Call) =>
            fieldAccess.argument.size shouldBe 2
            fieldAccess.code shouldBe "Bar.isStatic"
            fieldAccess.typeFullName shouldBe "int"
            fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

            val List(identifier: Identifier, field: FieldIdentifier) = fieldAccess.argument.l: @unchecked
            identifier.name shouldBe "Bar"
            identifier.typeFullName shouldBe "Bar"
            identifier.order shouldBe 1
            identifier.argumentIndex shouldBe 1

            field.canonicalName shouldBe "isStatic"
            field.code shouldBe "isStatic"
          }

          isStaticObjectInit.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"

          isAlsoStaticInit.methodFullName shouldBe Operators.assignment

          printStmt.methodFullName shouldBe "java.io.PrintStream.println:void(java.lang.String)"
      }
    }
  }
}
