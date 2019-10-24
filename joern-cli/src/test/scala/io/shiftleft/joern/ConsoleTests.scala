package io.shiftleft.joern

import better.files.File
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

class ConsoleTests extends WordSpec with Matchers with AbstractJoernCliTest {

  "should execute the list-funcs correctly for example code" in withTestCpg(
    File("joern-cli") / "src" / "test" / "resources" / "testcode" / "free") {
    case (cpg, outputFilename) =>
      val expected = cpg.method.name.l

      val actual = new TestConsole(outputFilename).runScript("list-funcs")
      actual shouldBe expected

      val actualT: List[String] = new TestConsole(outputFilename).runScriptT("list-funcs")
      actualT shouldBe expected
  }

}
