package io.shiftleft.joern

import better.files.File
import io.shiftleft.codepropertygraph.generated.traversal._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import io.shiftleft.semanticcpg.language._

/**
  * Test code that shows how code property graphs can be
  * generated using the FuzzyC language frontend
  * */
class GenerationTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "should generate and load CPG for example code" in withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))) {
    case (cpg, _) =>
      // Query to retrieve all method names
      cpg.method.name.l should not be empty
  }

}
