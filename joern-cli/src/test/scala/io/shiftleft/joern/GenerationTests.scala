package io.shiftleft.joern

import better.files.File
import org.scalatest.{Matchers, WordSpec}

import io.shiftleft.semanticcpg.language._

/**
  * Test code that shows how code property graphs can be
  * generated using the FuzzyC language frontend
  * */
class GenerationTests extends WordSpec with Matchers with AbstractJoernCliTest {

  "should generate and load CPG for example code" in withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))) {
    case (cpg, _) =>
      // Query to retrieve all method names
      cpg.method.name.l should not be empty
  }

}
