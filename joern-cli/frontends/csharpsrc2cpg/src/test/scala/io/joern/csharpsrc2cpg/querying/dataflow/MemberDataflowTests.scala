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

  "complete processing for cross file" in {
    val cpg = code("""
        |namespace Foo;
        |public class Bar {
        |   public string Username {get; set;} = string.Empty;
        |}
        |""".stripMargin).moreCode("""
        |using System.Threading;
        |namespace Baz;
        |
        |public class Fred<T>: SomeService where T:Bar {
        | private readonly ILogger _logger;
        | protected override Task ExecuteAsync(CancellationToken stoppingToken)
        |    {
        |        return Task.Run(
        |            async () =>
        |            {
        |                try
        |                {        
        |                        try
        |                        {}
        |                        catch (Exception ex)
        |                        {
        |                            _logger.LogErrorDemystified(ex, "Unexpected error when processing item");
        |                        }
        |                }
        |                catch (Exception ex)
        |                {
        |                    throw;
        |                }
        |            });
        |    }
        |}
        |""".stripMargin)

    val src  = cpg.member("Username").l
    val sink = cpg.call("LogErrorDemystified").l

    sink.reachableBy(src).size shouldBe 0
  }
}
