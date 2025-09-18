package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.passes.{AstCreationPass, OuterClassRefPass, TypeInferencePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{JavaConfigFileCreationPass, MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2CpgFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.matching.Regex

class JavaSrc2Cpg extends X2CpgFrontend {
  import JavaSrc2Cpg._

  override type ConfigType = Config
  override val defaultConfig = DefaultConfig

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()
      val astCreationPass = new AstCreationPass(config, cpg)
      astCreationPass.createAndApply()
      astCreationPass.sourceParser.cleanupDelombokOutput()
      astCreationPass.clearJavaParserCaches()
      new OuterClassRefPass(cpg).createAndApply()
      JavaConfigFileCreationPass(cpg, config = config).createAndApply()
      if (!config.skipTypeInfPass) {
        TypeNodePass.withRegisteredTypes(astCreationPass.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
        new TypeInferencePass(cpg).createAndApply()
      }
    }
  }

}

object JavaSrc2Cpg {
  val language: String                  = Languages.JAVASRC
  val sourceFileExtensions: Set[String] = Set(".java")

  val DefaultIgnoredFilesRegex: List[Regex] =
    List(".git", ".mvn", ".gradle", "test").map(Pattern.quote).flatMap { directory =>
      List(s"(^|\\\\)$directory($$|\\\\)".r.unanchored, s"(^|/)$directory($$|/)".r.unanchored)
    }
  val DefaultConfig: Config = Config()

  def showEnv(): Unit = {
    JavaSrcEnvVar.values.foreach { envVar =>
      val currentValue = sys.env.getOrElse(envVar.name, "<unset>")
      println(s"${envVar.name}:")
      println(s"\tDescription  : ${envVar.description}")
      println(s"\tCurrent value: $currentValue")
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
    case EnableVerboseTypeLogging
        extends JavaSrcEnvVar(
          "JAVASRC_ENABLE_VERBOSE_TYPE_LOGGING",
          "If set, javasrc2cpg will log all types found across JarTypeSolvers. THIS WILL IMPACT PERFORMANCE AND LOG SIZE AND SHOULD ONLY BE USED FOR DEBUGGING SPECIFIC ISSUES!"
        )
  }
}
