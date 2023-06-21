package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.{
  AstCreationPass,
  ConfigFileCreationPass,
  JavaTypeHintCallLinker,
  JavaTypeRecoveryPass,
  TypeInferencePass
}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass, XTypeRecoveryConfig}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.util.Try

class JavaSrc2Cpg extends X2CpgFrontend[Config] {
  import JavaSrc2Cpg._

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()
      val astCreationPass = new AstCreationPass(config, cpg)
      astCreationPass.createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      new TypeNodePass(astCreationPass.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
      new TypeInferencePass(cpg).createAndApply()
    }
  }

}

object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC
  private val logger   = LoggerFactory.getLogger(this.getClass)

  val sourceFileExtensions: Set[String] = Set(".java")
  def apply(): JavaSrc2Cpg              = new JavaSrc2Cpg()

  def typeRecoveryPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = {
    List(
      new JavaTypeRecoveryPass(cpg, XTypeRecoveryConfig(enabledDummyTypes = !config.exists(_.disableDummyTypes))),
      new JavaTypeHintCallLinker(cpg)
    )
  }
}
