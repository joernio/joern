package io.joern.x2cpg

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Using

class JoernTITests extends AnyWordSpec with Matchers {

  "test spawning a JoernTI process and connect" in {
    Using.resource(new JoernTI(spawnProcess = true)) { joernti =>
      println("Success!")
    }
  }

}
