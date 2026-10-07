package io.joern.console

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunTests extends AnyWordSpec with Matchers {

  "the code for the run command" should {
    "offer the layers that take options" in {
      Run.codeForRunCommand() should include("_root_.io.joern.x2cpg.layers.Base")
    }

    "not offer the post-processing layer of a frontend" in {
      Run.codeForRunCommand() should not include "frontendspecific"
    }
  }

}
