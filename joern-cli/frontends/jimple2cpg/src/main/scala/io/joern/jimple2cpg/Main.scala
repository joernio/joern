package io.joern.jimple2cpg

import io.joern.jimple2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.util.concurrent.ExecutorService

/** Command line configuration parameters
  */
final case class Config(
  android: Option[String] = None,
  dynamicDirs: Seq[String] = Seq.empty,
  dynamicPkgs: Seq[String] = Seq.empty,
  fullResolver: Boolean = false,
  recurse: Boolean = false,
  depth: Int = 1,
  override val sharedConfig: X2CpgConfig.SharedConfig = X2CpgConfig.SharedConfig()
) extends X2CpgConfig[Config] {
  override def withSharedConfig(newSharedConfig: X2CpgConfig.SharedConfig): Config =
    copy(sharedConfig = newSharedConfig)

  def withAndroid(android: String): Config = {
    copy(android = Some(android))
  }
  def withDynamicDirs(value: Seq[String]): Config = {
    copy(dynamicDirs = value)
  }
  def withDynamicPkgs(value: Seq[String]): Config = {
    copy(dynamicPkgs = value)
  }
  def withFullResolver(value: Boolean): Config = {
    copy(fullResolver = value)
  }
  def withRecurse(value: Boolean): Config = {
    copy(recurse = value)
  }
  def withDepth(value: Int): Config = {
    copy(depth = value)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("jimple2cpg"),
      opt[String]("android")
        .text("Optional path to android.jar while processing apk file.")
        .action((android, config) => config.withAndroid(android)),
      opt[Unit]("full-resolver")
        .text("enables full transitive resolution of all references found in all classes that are resolved")
        .action((_, config) => config.withFullResolver(true)),
      opt[Unit]("recurse")
        .text("recursively unpack jars")
        .action((_, config) => config.withRecurse(true)),
      opt[Int]("depth")
        .text("maximum depth to recursively unpack jars, default value 1")
        .action((depth, config) => config.withDepth(depth))
        .validate(x => if (x > 0) success else failure("depth must be greater than 0")),
      opt[Seq[String]]("dynamic-dirs")
        .valueName("<dir1>,<dir2>,...")
        .text(
          "Mark all class files in dirs as classes that may be loaded dynamically. Comma separated values for multiple directories."
        )
        .action((dynamicDirs, config) => config.withDynamicDirs(dynamicDirs)),
      opt[Seq[String]]("dynamic-pkgs")
        .valueName("<pkg1>,<pkg2>,...")
        .text(
          "Marks all class files belonging to the package pkg or any of its subpackages as classes which the application may load dynamically. Comma separated values for multiple packages."
        )
        .action((dynamicPkgs, config) => config.withDynamicPkgs(dynamicPkgs))
    )
  }
}

/** Entry point for command line CPG creator
  */
object Main extends X2CpgMain(new Jimple2Cpg(), cmdLineParser) with FrontendHTTPServer {

  override protected val executor: ExecutorService = FrontendHTTPServer.singleThreadExecutor()

  def run(config: frontend.ConfigType): Unit = {
    if (config.serverMode) { startup(); config.serverTimeoutSeconds.foreach(serveUntilTimeout) }
    else { frontend.run(config) }
  }
}
