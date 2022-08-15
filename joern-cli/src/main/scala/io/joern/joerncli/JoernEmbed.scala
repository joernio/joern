package io.joern.joerncli

object JoernEmbed extends App {

  case class Config(cpgFileName: String = "cpg.bin", outDir: String = "out")

  private def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-export") {
      head("Dump intermediate graph representations (or entire graph) of code in a given export format")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]('o', "out")
        .text("output directory - will be created and must not yet exist")
        .action((x, c) => c.copy(outDir = x))
    }.parse(args, Config())

  parseConfig.foreach { config =>
    println(config)
  }

}
