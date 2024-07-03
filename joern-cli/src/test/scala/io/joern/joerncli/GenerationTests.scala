package io.joern.joerncli

import better.files.File
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Test code that shows how code property graphs can be generated using the c2cpg language frontend */
class GenerationTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "should generate and load CPG for example code" in withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))
  ) { case (cpg, _) =>
    // Query to retrieve all method names
    cpg.method.name.l should not be empty
  }

}
