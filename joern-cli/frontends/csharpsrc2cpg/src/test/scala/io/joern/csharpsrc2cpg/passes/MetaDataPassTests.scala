package io.joern.csharpsrc2cpg.passes

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MetaDataPassTests extends CSharpCode2CpgFixture {

  "a .cs file on the project root" should {

    val cpg = code(basicBoilerplate(), "Program.cs")

    "create a meta data node" in {
      val metadata = cpg.metaData.head

      metadata.language shouldBe "CSHARPSRC"
      metadata.root should not be empty
      metadata.hash match
        case Some(hash) => hash should startWith("bd9afc24308")
        case None       => fail("No input hash detected")
    }

  }

}
