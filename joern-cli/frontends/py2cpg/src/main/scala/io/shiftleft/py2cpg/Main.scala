package io.shiftleft.py2cpg

import org.rogach.scallop._

class ParsedArguments(arguments: Seq[String]) extends ScallopConf(arguments) {
  val output = opt[String](
    default = Some("out.cpg"),
    short = 'o',
    descr = "Output file name. Defaults to out.cpg"
  )

  val input = trailArg[String](descr = "Input file or directory name")
  verify()
}

object Main extends App {
  val parsedArguments = new ParsedArguments(args)

  val py2CpgConfig =
    new Py2CpgOnFileSystemConfig(parsedArguments.output.toOption.get, parsedArguments.input.toOption.get)

  Py2CpgOnFileSystem.buildCpg(py2CpgConfig)
}
