package io.shiftleft.joern.server

import org.scalatest.{Matchers, WordSpec}

class JoernServerImplTests extends WordSpec with Matchers {

  val impl = new JoernServerImpl

  "createCpg should work" in {
    impl.createCpg(List("joern-cli/src/test/resources/testcode/free"))
    impl.cpg.isDefined shouldBe true
  }

  // TODO we are currently missing a test for `query` because of
  // https://github.com/scala/bug/issues/10058

}
