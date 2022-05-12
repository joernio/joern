package io.joern.x2cpg.utils

import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HashUtilsTest extends AnyWordSpec with Matchers {

  "generate sha256 hash" in {
    val testFile = ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/testfile")
    HashUtil.sha256(testFile) shouldBe "5881707e54b0112f901bc83a1ffbacac8fab74ea46a6f706a3efc5f7d4c1c625"
  }

}
