package io.joern.joerncli

import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

/** Test code that shows how code property graphs can be generated using the c2cpg language frontend */
class GenerationTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "should generate and load CPG for example code" in withTestCpg(
    Paths.get(getClass.getClassLoader.getResource("testcode/free").toURI)
  ) { case (cpg, _) =>
    // Query to retrieve all method names
    cpg.method.name.l should not be empty
  }

}
