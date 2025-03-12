package io.joern.console

import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class ConsoleConfigTest extends AnyWordSpec with Matchers {
  "An InstallConfig" should {
    "set the rootPath to directory containing `.installation_root` by default" in {
      val config = new InstallConfig(environment = Map.empty)
      config.rootPath shouldBe ProjectRoot.find.path
    }

    "set the rootPath to SHIFTLEFT_OCULAR_INSTALL_DIR if it is defined" in {
      val config = new InstallConfig(environment = Map("SHIFTLEFT_OCULAR_INSTALL_DIR" -> "/tmp"))
      config.rootPath shouldBe Paths.get("/tmp")
    }

    "copy config with params correctly" in {
      val initialParamList    = List("param1", "param2")
      val additionalParamList = List("param3", "param4", "param5")

      val config     = new FrontendConfig(initialParamList)
      val copyConfig = config.withArgs(additionalParamList)

      withClue("should be able to copy config without mutating original") {
        copyConfig.cmdLineParams shouldBe (initialParamList ++ additionalParamList)
      }

      withClue("should be able to mutate config") {
        val moreAdditionalParams = List("param5", "param6")
        config.cmdLineParams ++= moreAdditionalParams
        config.cmdLineParams shouldBe (initialParamList ++ moreAdditionalParams)
      }
    }
  }
}
