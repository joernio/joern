package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
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

}
