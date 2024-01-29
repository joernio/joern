package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class MemberDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {
  "member dataflows" should {
    val cpg = code(
      """public class Car
        |{
        |  string color;                
        |  static int maxSpeed = 200;
        |  public void fullThrottle()
        |  {
        |    Console.WriteLine(color);
        |  }
        |}
        |""".stripMargin,
      "Car.cs"
    )

    "find a path from member to WriteLine call" in {
      val src  = cpg.member.nameExact("color").l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableBy(src).size shouldBe 1
    }
  }
}
