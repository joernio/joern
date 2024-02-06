package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class MemberTests extends CSharpCode2CpgFixture {

  "a basic class declaration" should {
    val cpg = code(
      """public class Car
        |{
        |  string color;                // field
        |  static int maxSpeed = 200;   // field
        |  public void fullThrottle()   // method
        |  {
        |    Console.WriteLine("The car is going as fast as it can!");
        |  }
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "generate members for fields" in {
      val x = cpg.typeDecl.nameExact("Car").head

      val color = x.member.nameExact("color").head
      color.typeFullName shouldBe "System.String"
      color.code shouldBe "string color"
      color.modifier.modifierType.l shouldBe ModifierTypes.INTERNAL :: Nil

      val maxSpeed = x.member.nameExact("maxSpeed").head
      maxSpeed.typeFullName shouldBe "System.Int32"
      maxSpeed.code shouldBe "int maxSpeed = 200"
      maxSpeed.modifier.modifierType.l shouldBe ModifierTypes.INTERNAL :: ModifierTypes.STATIC :: Nil
    }

  }

  "a basic class declaration with a static constructor" should {
    val cpg = code(
      """public class Car
        |{
        |  string color;                // field
        |  static int maxSpeed = 200;   // field
        |  static int nonInitMaxSpeed;  // field
        |
        |  public void fullThrottle()   // method
        |  {
        |    Console.WriteLine("The car is going as fast as it can!");
        |  }
        |
        |  static Car() { // static constructor
        |     nonInitMaxSpeed = 2000;
        |  }
        |
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "generate one static constructor" in {
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.StaticInitMethodName).l) {
        case m :: Nil =>
          m.fullName shouldBe s"Car.${Defines.StaticInitMethodName}:void()"
          m.modifier.modifierType.l shouldBe ModifierTypes.STATIC :: ModifierTypes.CONSTRUCTOR :: Nil
          m.methodReturn.typeFullName shouldBe "void"

          inside(m.assignment.l) {
            case maxSpeed :: nonInitMaxSpeed :: Nil =>
              maxSpeed.code shouldBe "maxSpeed = 200"
              nonInitMaxSpeed.code shouldBe "nonInitMaxSpeed = 2000"
            case _ => fail("Exactly 2 assignments expected")
          }
        case _ => fail("`Car` has no static initializer method")
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
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) {
        case m :: Nil =>
          m.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:void()"
          m.modifier.modifierType.l shouldBe ModifierTypes.PUBLIC :: ModifierTypes.CONSTRUCTOR :: Nil
          m.methodReturn.typeFullName shouldBe "void"

          inside(m.assignment.l) {
            case color :: initMaxSpeed :: Nil =>
              color.code shouldBe "color = \"abc\""
              initMaxSpeed.code shouldBe "initMaxSpeed = 220"
            case _ => fail("Exactly 2 assignments expected")
          }
        case _ => fail("`Car` has no constructor initializer method")
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
      inside(cpg.typeDecl.nameExact("Car").method.nameExact(Defines.ConstructorMethodName).l) {
        case m :: Nil =>
          m.fullName shouldBe s"Car.${Defines.ConstructorMethodName}:void(System.Int32)"
          m.modifier.modifierType.l shouldBe ModifierTypes.PUBLIC :: ModifierTypes.CONSTRUCTOR :: Nil
          m.methodReturn.typeFullName shouldBe "void"

          inside(m.assignment.l) {
            case color :: initMaxSpeed :: Nil =>
              color.code shouldBe "color = \"abc\""
              initMaxSpeed.code shouldBe "initMaxSpeed = 220"
            case _ => fail("Exactly 2 assignments expected")
          }

          inside(m.parameter.l) {
            case thisVariable :: minSpeed :: Nil =>
              thisVariable.code shouldBe "this"
              thisVariable.typeFullName shouldBe "Car"

              minSpeed.code shouldBe "int minSpeed"
              minSpeed.typeFullName shouldBe "System.Int32"
            case _ => fail("Exactly 2 parameters expected for constructor")
          }
        case _ => fail("`Car` has no constructor initializer method")
      }
    }
  }
}
