package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class ControlStructureDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {

  "if data-flows" should {
    val cpg = code(basicBoilerplate("""
        |var b = true;
        |var s = "MALICIOUS";
        |if (b) {
        |  s = "SAFE";
        |}
        |Console.WriteLine(s);
        |""".stripMargin))

    "find a path if `MALICIOUS` is reassigned to `SAFE` in only one path of an if" in {
      val src  = cpg.literal.codeExact("\"MALICIOUS\"").l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableBy(src).size shouldBe 1
    }
  }

  "foreach dataflows" should {
    val cpg = code(basicBoilerplate("""
        |List<int> fibNumbers = [0, 1, 1, 2, 3, 5, 8, 13];
        |int i = 0;
        |foreach (int element in fibNumbers)
        |{
        |    Console.Write(element);
        |    i++;
        |}
        |int newI = i + 1;
        |""".stripMargin))

    "find a path from element to Write and from i to assignment through a foreach loop" in {
      val elementSrc = cpg.identifier.nameExact("element").l
      val writeSink  = cpg.call.nameExact("Write").l
      writeSink.reachableBy(elementSrc).size shouldBe 2

      val assignmentSrc = cpg.identifier.nameExact("i").lineNumber(10).l
      val newI          = cpg.identifier.nameExact("newI").l
      newI.reachableBy(assignmentSrc).size shouldBe 1

      cpg.assignment.codeExact("i++").l.reachableBy(assignmentSrc).size shouldBe 1
    }
  }

  "for dataflows" should {
    val cpg = code(basicBoilerplate("""
        |val name = "John Doe";
        |for (int i = 0; i < 10;i++) {
        | Console.Write(name);
        | val newName = name + "a";
        |}
        |val newName = name;
        |""".stripMargin))

    "find a path from name to write call through a for loop" in {
      val src  = cpg.identifier.nameExact("name").l
      val sink = cpg.identifier.nameExact("newName").lineNumber(14).l
      sink.reachableBy(src).size shouldBe 4

      cpg.identifier.nameExact("newName").lineNumber(12).l.reachableBy(src).size shouldBe 3
    }
  }

  "try-catch-finally dataflows" should {
    val cpg = code(basicBoilerplate("""
        |var Busy = true;
        |try
        |{
        |  Console.Write(Busy);
        |}
        |catch (Exception e)
        |{
        |  Console.WriteLine("Uh, oh!");
        |} finally
        |{
        | var newBusy = Busy;
        |}
        |""".stripMargin))

    "find a path from identifier to another identifier through try-catch-finally" in {
      val src            = cpg.identifier.nameExact("Busy").lineNumber(9).l
      val writeSink      = cpg.call.nameExact("Write").l
      val identifierSink = cpg.identifier.nameExact("newBusy").l
      writeSink.reachableBy(src).size shouldBe 1
      identifierSink.reachableBy(src).size shouldBe 1
    }
  }
}
