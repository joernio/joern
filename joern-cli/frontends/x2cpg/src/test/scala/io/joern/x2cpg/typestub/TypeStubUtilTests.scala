package io.joern.x2cpg.typestub

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeStubUtilTests extends AnyWordSpec with Matchers {

  "typeStubDir" should {

    "resolve code source locations containing unescaped spaces" in {
      // install paths like `file:/Users/John Doe/...` -- `URI.create` rejects raw spaces
      TypeStubUtil
        .typeStubDir("file:/Users/John Doe/joern/lib/rubysrc2cpg_3.jar")
        .toString should endWith("type_stubs")
    }

    "resolve Windows-style code source locations" in {
      TypeStubUtil
        .typeStubDir("file:/D:/a/joern/joern/lib/rubysrc2cpg_3.jar")
        .toString should endWith("type_stubs")
    }
  }

}
