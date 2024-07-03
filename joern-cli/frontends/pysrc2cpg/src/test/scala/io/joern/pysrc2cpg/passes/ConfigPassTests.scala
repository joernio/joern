package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ConfigPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "The CPG for a `requirements.txt` file" should {
    lazy val cpg = code("Flask==1.1.2", "requirements.txt")
    "contain a CONFIG_FILE node" in {
      val List(c) = cpg.configFile.l
      c.content shouldBe "Flask==1.1.2"
      c.name shouldBe "requirements.txt"
    }

  }

}
