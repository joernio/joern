package io.shiftleft.joern

case class FlowConfig(cpgFileName: String = "cpg.bin",
                      srcRegex: String = ".*",
                      dstRegex: String = ".*",
                      srcParam: Option[Int] = None,
                      dstParam: Option[Int] = None)

object JoernFlow extends App {

  private def parseConfig: Option[FlowConfig] = {
    new scopt.OptionParser[FlowConfig]("joern-flow") {
      head("Find flows")
      help("help")

      arg[String]("src")
        .text("source regex")
        .action((x, c) => c.copy(srcRegex = x))

      arg[String]("dst")
        .text("destination regex")
        .action((x, c) => c.copy(dstRegex = x))

      arg[Int]("srcParam")
        .text("Source parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

      arg[Int]("dstParam")
        .text("Destination parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

      arg[String]("cpg")
        .text("CPG file name ('cpg.bin' by default)")
        .optional()
        .action((x, c) => c.copy(srcRegex = x))
    }
  }.parse(args, FlowConfig())

  parseConfig.foreach { config =>
    print("Loading graph... ")
    val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
    println("[DONE]")

    cpg.close()
  }

}
