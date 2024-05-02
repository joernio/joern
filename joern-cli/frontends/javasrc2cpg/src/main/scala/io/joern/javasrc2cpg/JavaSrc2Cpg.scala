package io.joern.javasrc2cpg

import better.files.File
import io.joern.javasrc2cpg.passes.{
  AstCreationPass,
  ConfigFileCreationPass,
  JavaTypeHintCallLinker,
  JavaTypeRecoveryPassGenerator,
  TypeInferencePass
}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass, XTypeRecoveryConfig}
import io.joern.x2cpg.X2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

class JavaSrc2Cpg extends X2CpgFrontend[Config] {
  import JavaSrc2Cpg._

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()
      val astCreationPass = new AstCreationPass(config, cpg)
      astCreationPass.createAndApply()
      astCreationPass.sourceParser.cleanupDelombokOutput()
      astCreationPass.clearJavaParserCaches()
      new ConfigFileCreationPass(cpg).createAndApply()
      if (!config.skipTypeInfPass) {
        TypeNodePass.withRegisteredTypes(astCreationPass.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
        new TypeInferencePass(cpg).createAndApply()
      }
    }
  }

}

object JavaSrc2Cpg {
  val language: String = Languages.JAVASRC
  private val logger   = LoggerFactory.getLogger(this.getClass)

  val sourceFileExtensions: Set[String] = Set(".java")

  val DefaultIgnoredFilesRegex: List[Regex] = List(".git", ".mvn", "test").flatMap { directory =>
    List(s"(^|\\\\)$directory($$|\\\\)".r.unanchored, s"(^|/)$directory($$|/)".r.unanchored)
  }

  val DefaultConfig: Config =
    Config().withDefaultIgnoredFilesRegex(DefaultIgnoredFilesRegex)

  def apply(): JavaSrc2Cpg = new JavaSrc2Cpg()

  def typeRecoveryPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = {
    new JavaTypeRecoveryPassGenerator(cpg, XTypeRecoveryConfig(enabledDummyTypes = !config.exists(_.disableDummyTypes)))
      .generate() ++
      List(new JavaTypeHintCallLinker(cpg))
  }

  def showEnv(): Unit = {
    val value =
      JavaSrcEnvVar.values.foreach { envVar =>
        val currentValue = Option(System.getenv(envVar.name)).getOrElse("<unset>")
        println(s"${envVar.name}:")
        println(s"  Description  : ${envVar.description}")
        println(s"  Current value: $currentValue")
      }
  }

  enum JavaSrcEnvVar(val name: String, val description: String) {
    case JdkPath
        extends JavaSrcEnvVar(
          "JAVASRC_JDK_PATH",
          "Path to the JDK home used for retrieving type information about builtin Java types."
        )
    case FetchDependencies
        extends JavaSrcEnvVar(
          "JAVASRC_FETCH_DEPENDENCIES",
          "If set, javasrc2cpg will fetch dependencies regardless of the --fetch-dependencies flag."
        )
  }
}
