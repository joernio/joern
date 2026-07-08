package io.joern.lua2cpg

import io.joern.lua2cpg.passes.{LuaBytecodeModelPass, LuaFileInventoryPass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.util.Try

class Lua2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      new MetaDataPass(cpg, "LUA", config.inputPath).createAndApply()
      new LuaFileInventoryPass(cpg, config).createAndApply()
      new LuaBytecodeModelPass(cpg, config).createAndApply()
    }
  }
}
