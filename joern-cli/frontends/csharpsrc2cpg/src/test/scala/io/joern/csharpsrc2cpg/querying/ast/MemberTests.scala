package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class MemberTests extends CSharpCode2CpgFixture {

  "class with static and non-static members" should {
    val cpg = code("""
        |class Car
        |{
        |   string color;
        |   static int maxSpeed = 200;
        |}""".stripMargin)

    "have the non-static member correctly set" in {
      inside(cpg.member.nameExact("color").l) { case color :: Nil =>
        color.typeFullName shouldBe "System.String"
        color.code shouldBe "string color"
        color.modifier.modifierType.l shouldBe List(ModifierTypes.INTERNAL)
      }
    }

    "have the static member correctly set`" in {
      inside(cpg.member.nameExact("maxSpeed").l) { case maxSpeed :: Nil =>
        maxSpeed.typeFullName shouldBe "System.Int32"
        maxSpeed.code shouldBe "int maxSpeed = 200"
        maxSpeed.modifier.modifierType.toSet shouldBe Set(ModifierTypes.INTERNAL, ModifierTypes.STATIC)
      }
    }
  }

  "class with initialized static member" should {
    val cpg = code("""
        |class Car
        |{
        | static int nonInitMaxSpeed = 200;
        |}
        |""".stripMargin)

    "have the static member correctly set" in {
      inside(cpg.member.nameExact("nonInitMaxSpeed").l) { case nonInitMaxSpeed :: Nil =>
        nonInitMaxSpeed.typeFullName shouldBe "System.Int32"
        nonInitMaxSpeed.code shouldBe "int nonInitMaxSpeed = 200"
        nonInitMaxSpeed.modifier.modifierType.l shouldBe List(ModifierTypes.INTERNAL, ModifierTypes.STATIC)
      }
    }

    "have a static constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).l) { case cctor :: Nil =>
        cctor.fullName shouldBe s"Car.${Defines.StaticInitMethodName}:System.Void()"
        cctor.modifier.modifierType.toSet shouldBe Set(
          ModifierTypes.STATIC,
          ModifierTypes.CONSTRUCTOR,
          ModifierTypes.INTERNAL
        )
        cctor.methodReturn.typeFullName shouldBe "System.Void"
      }
    }

    "have the static member initialization inside the static constructor" in {
      inside(cpg.method.fullNameExact(s"Car.${Defines.StaticInitMethodName}:System.Void()").body.assignment.l) {
        case assignment :: Nil =>
          assignment.target.code shouldBe "nonInitMaxSpeed"
          assignment.source.code shouldBe "200"
      }
    }
  }

  "class with initialized member" should {
    val cpg = code("""
        |class Car
        |{
        | string color = "red";
        |}
        |""".stripMargin)

    "have the member correctly set" in {
      inside(cpg.member.nameExact("color").l) { case color :: Nil =>
        color.typeFullName shouldBe "System.String"
        color.code shouldBe "string color = \"red\""
        color.modifier.modifierType.l shouldBe List(ModifierTypes.INTERNAL)
      }
    }

    "have a constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) { case ctor :: Nil =>
        ctor.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:System.Void()"
        ctor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.INTERNAL, ModifierTypes.CONSTRUCTOR)
        ctor.methodReturn.typeFullName shouldBe "System.Void"
      }
    }

    "have the member initialization inside the constructor" in {
      inside(cpg.method.fullNameExact(s"Car.${Defines.ConstructorMethodName}:System.Void()").body.assignment.l) {
        case assignment :: Nil =>
          assignment.target.code shouldBe "color"
          assignment.source.code shouldBe "\"red\""
      }
    }
  }

  "class with static constructor" should {
    val cpg = code("""
        |class Car
        |{
        | static Car()
        | {
        | }
        |}""".stripMargin)
    "have a static constructor correctly set" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).l) { case cctor :: Nil =>
        cctor.fullName shouldBe s"Car.${Defines.StaticInitMethodName}:System.Void()"
        cctor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.STATIC, ModifierTypes.CONSTRUCTOR)
        cctor.methodReturn.typeFullName shouldBe "System.Void"
      }
    }
  }

  "class with static constructor and initialized static member" should {
    val cpg = code("""
        |class Car
        |{
        | static int maxSpeed = 200;
        | static Car()
        | {
        | }
        |}""".stripMargin)
    "have static member initialization inside static constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).body.assignment.l) {
        case assignment :: Nil =>
          assignment.code shouldBe "maxSpeed = 200"
          assignment.source.code shouldBe "200"
          // TODO: target is currently an identifier. Should it be `Car.maxSpeed` instead?
          assignment.target.code shouldBe "maxSpeed"
      }
    }
  }

  "class with static constructor initializing a member, plus an initialized static member" should {
    val cpg = code("""
        |class Car
        |{
        | static int maxSpeed = 200;
        | static int nonInitMaxSpeed;
        | static Car()
        | {
        |   nonInitMaxSpeed = 300;
        | }
        |}
        |""".stripMargin)
    "have static constructor with two assignments for initializing the members" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).assignment.sortBy(_.code).l) {
        case maxSpeedAssignment :: nonInitMaxSpeedAssignment :: Nil =>
          maxSpeedAssignment.code shouldBe "maxSpeed = 200"
          nonInitMaxSpeedAssignment.code shouldBe "nonInitMaxSpeed = 300"

          // TODO: They should have the same representation
          maxSpeedAssignment.target.code shouldBe "maxSpeed"
          nonInitMaxSpeedAssignment.target.code shouldBe "Car.nonInitMaxSpeed"
      }
    }
  }

  "class with initialized member and default constructor" should {
    val cpg = code("""
        |class Car
        |{
        | string color = "red";
        | Car()
        | {
        | }
        |}""".stripMargin)
    "have a constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) { case ctor :: Nil =>
        ctor.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:System.Void()"
        ctor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.CONSTRUCTOR)
        ctor.methodReturn.typeFullName shouldBe "System.Void"
      }
    }

    "have the member initialization inside the constructor" in {
      inside(cpg.method.fullNameExact(s"Car.${Defines.ConstructorMethodName}:System.Void()").body.assignment.l) {
        case assignment :: Nil =>
          // TODO: test LHS: shouldn't it resemble `this.color`?
          assignment.target.code shouldBe "color"
          assignment.source.code shouldBe "\"red\""
      }
    }
  }

  "a basic class declaration with a default constructor" should {
    val cpg = code(
      """public class Car
        |{
        |  string color = "abc";                // field
        |  static int maxSpeed = 200;   // field
        |  int initMaxSpeed;  // field
        |  static int staticInitMaxSpeed; // field
        |
        |  public Car() {
        |    initMaxSpeed = 220;
        |  }
        |
        |  public void fullThrottle()   // method
        |  {
        |    Console.WriteLine("The car is going as fast as it can!");
        |  }
        |
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "generate one constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) { case m :: Nil =>
        m.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:System.Void()"
        m.modifier.modifierType.l shouldBe ModifierTypes.PUBLIC :: ModifierTypes.CONSTRUCTOR :: Nil
        m.methodReturn.typeFullName shouldBe "System.Void"

        inside(m.assignment.l) { case color :: initMaxSpeed :: Nil =>
          color.code shouldBe "color = \"abc\""
          initMaxSpeed.code shouldBe "initMaxSpeed = 220"
        }
      }
    }
  }

  "a basic class declaration with a constructor with parameters" should {
    val cpg = code(
      """public class Car
        |{
        |  string color = "abc";                // field
        |  static int maxSpeed = 200;   // field
        |  int initMaxSpeed;  // field
        |  static int staticInitMaxSpeed; // field
        |
        |  public Car(int minSpeed) {
        |    initMaxSpeed = 220;
        |  }
        |
        |  public void fullThrottle()   // method
        |  {
        |    Console.WriteLine("The car is going as fast as it can!");
        |  }
        |
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "generate one constructor with necessary parameters" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) { case m :: Nil =>
        m.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:System.Void(System.Int32)"
        m.modifier.modifierType.l shouldBe ModifierTypes.PUBLIC :: ModifierTypes.CONSTRUCTOR :: Nil
        m.methodReturn.typeFullName shouldBe "System.Void"

        inside(m.assignment.l) { case color :: initMaxSpeed :: Nil =>
          color.code shouldBe "color = \"abc\""
          initMaxSpeed.code shouldBe "initMaxSpeed = 220"
        }

        inside(m.parameter.l) { case thisVariable :: minSpeed :: Nil =>
          thisVariable.code shouldBe "this"
          thisVariable.typeFullName shouldBe "Car"

          minSpeed.code shouldBe "int minSpeed"
          minSpeed.typeFullName shouldBe "System.Int32"
        }
      }
    }
  }

  "a basic class declaration with a static constructor and explicit fieldAccess" should {
    val cpg = code(
      """public class Car
        |{
        |  string color;                // field
        |  static int maxSpeed = 200;   // field
        |  static int nonInitMaxSpeed;  // field
        |  int b;
        |
        |  static Car() { // static constructor
        |     nonInitMaxSpeed = 2000;
        |  }
        |
        |  public Car() {
        |     this.color = "red";
        |     b = 1000;
        |     b++;
        |  }
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "generate one static constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).l) { case m :: Nil =>
        inside(m.body.astChildren.isCall.l) { case staticImplicit :: staticExplicit :: Nil =>
          staticExplicit.methodFullName shouldBe Operators.assignment
          staticExplicit.code shouldBe "nonInitMaxSpeed = 2000"

          inside(staticExplicit.argument.fieldAccess.l) { case fieldAccess :: Nil =>
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "Car.nonInitMaxSpeed"
            fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
            fieldAccess.typeFullName shouldBe "System.Int32"

          }

          staticImplicit.methodFullName shouldBe Operators.assignment
          staticImplicit.code shouldBe "maxSpeed = 200"

        }
      }
    }

    "generate one constructor with correct field access" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) { case m :: Nil =>
        inside(m.body.astChildren.isCall.l) {
          case explicitFieldAccess :: implicitFieldAccessAssignment :: implicitFieldAccessUnary :: Nil =>
            explicitFieldAccess.methodFullName shouldBe Operators.assignment
            explicitFieldAccess.code shouldBe "this.color = \"red\""

            implicitFieldAccessAssignment.methodFullName shouldBe Operators.assignment
            implicitFieldAccessAssignment.code shouldBe "b = 1000"

            implicitFieldAccessUnary.methodFullName shouldBe Operators.postIncrement
            implicitFieldAccessUnary.code shouldBe "b++"

            inside(explicitFieldAccess.argument.fieldAccess.l) { case fieldAccessNode :: Nil =>
              fieldAccessNode.methodFullName shouldBe Operators.fieldAccess
              fieldAccessNode.code shouldBe "this.color"
              fieldAccessNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              fieldAccessNode.typeFullName shouldBe "System.String"
            }

            inside(implicitFieldAccessAssignment.argument.fieldAccess.l) { case fieldAccessNode :: Nil =>
              fieldAccessNode.methodFullName shouldBe Operators.fieldAccess
              fieldAccessNode.code shouldBe "this.b"
              fieldAccessNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              fieldAccessNode.typeFullName shouldBe "System.Int32"
            }

            inside(implicitFieldAccessUnary.argument.fieldAccess.l) { case fieldAccessNode :: Nil =>
              fieldAccessNode.methodFullName shouldBe Operators.fieldAccess
              fieldAccessNode.code shouldBe "this.b"
              fieldAccessNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              fieldAccessNode.typeFullName shouldBe "System.Int32"
            }

        }
      }
    }
  }

  "a basic class declaration with a local variable shadowing a field" should {
    val cpg = code("""
        |public class Foo {
        |  int a;
        |
        |  public Foo() {
        |    var a = 2;
        |    this.a = a;
        |  }
        |}
        |""".stripMargin)

    "create a non field access node" in {
      inside(cpg.typeDecl.nameExact("Foo").method.nameExact(Defines.ConstructorMethodName).l) {
        case fooConstructor :: Nil =>
          inside(fooConstructor.body.astChildren.isCall.assignment.l) { case localCall :: thisCall :: Nil =>
            localCall.target.fieldAccess.size shouldBe 0
            thisCall.target.fieldAccess.size shouldBe 1

          }
      }
    }
  }

  // TODO: Getters/Setters are currently being lowered into get_/set_ methods.
  //  Adapt this unit-test once that is finished.
  "a basic class declaration with a PropertyDeclaration member" ignore {
    val cpg = code("""
        |public class Foo {
        | public int Bar {get; set;}
        |}
        |""".stripMargin)

    "create a member for Bar with appropriate properties" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case fooClass :: Nil =>
        inside(fooClass.astChildren.isMember.nameExact("Bar").l) { case bar :: Nil =>
          bar.code shouldBe "public int Bar"
          bar.typeFullName shouldBe "System.Int32"
          bar.astParent shouldBe fooClass
        }
      }
    }
  }

  "a basic class declaration with a FieldDeclaration member" should {
    val cpg = code("""
        |public class Foo {
        | public int Bar;
        |}
        |""".stripMargin)

    "create a member for Bar with appropriate properties" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case fooClass :: Nil =>
        inside(fooClass.astChildren.isMember.nameExact("Bar").l) { case bar :: Nil =>
          bar.code shouldBe "int Bar"
          bar.typeFullName shouldBe "System.Int32"
          bar.astParent shouldBe fooClass
        }
      }
    }
  }

  "a member with a external type" should {
    val cpg = code("""
        |using Microsoft.Extensions.Logging;
        |
        |namespace Foo {
        | public class Bar {
        |   private readonly ILogger<AcceptBookingRequestHandler> _logger;
        |
        |   public static void Main() {
        |     _logger.LogInformation("Some info");
        |   }
        | }
        |
        |}
        |""".stripMargin)

    "link to it's reference, and propagate type" in {
      inside(cpg.identifier.nameExact("_logger").l) { case logger :: Nil =>
        logger.typeFullName shouldBe "Microsoft.Extensions.Logging.ILogger"
      }
    }
  }
}
