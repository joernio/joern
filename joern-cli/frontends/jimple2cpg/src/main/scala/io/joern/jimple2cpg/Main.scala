package io.joern.jimple2cpg

import io.joern.jimple2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  android: Option[String] = None,
  dynamicDirs: Seq[String] = Seq.empty,
  dynamicPkgs: Seq[String] = Seq.empty,
  fullResolver: Boolean = false,
  recurse: Boolean = false,
  depth: Int = 1
) extends X2CpgConfig[Config] {
  def withAndroid(android: String): Config = {
    copy(android = Some(android)).withInheritedFields(this)
  }
  def withDynamicDirs(value: Seq[String]): Config = {
    copy(dynamicDirs = value).withInheritedFields(this)
  }
  def withDynamicPkgs(value: Seq[String]): Config = {
    copy(dynamicPkgs = value).withInheritedFields(this)
  }
  def withFullResolver(value: Boolean): Config = {
    copy(fullResolver = value).withInheritedFields(this)
  }
  def withRecurse(value: Boolean): Config = {
    copy(recurse = value).withInheritedFields(this)
  }
  def withDepth(value: Int): Config = {
    copy(depth = value).withInheritedFields(this)
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
object Main extends X2CpgMain(cmdLineParser, new Jimple2Cpg()) {
  def run(config: Config, jimple2Cpg: Jimple2Cpg): Unit = {
    jimple2Cpg.run(config)
  }
}
