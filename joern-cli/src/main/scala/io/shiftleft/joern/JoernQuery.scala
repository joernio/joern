package io.shiftleft.joern

import java.io.FileReader
import javax.script.ScriptEngineManager

object JoernQuery extends App {

  parseConfig.foreach { config =>
    val executor = new JoernScriptExecutor(config.cpgFilename)

    if (config.isFile) {
      val reader = new FileReader(config.query)
      println(executor.run(reader))
    } else {
      val script = config.query + ".l.mkString(\"\\n\")"
      println(executor.run(script))
    }
  }

  case class Config(cpgFilename: String, query: String, isFile: Boolean = false)
  def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-query") {
      opt[String]('c', "cpg")
        .text("the code property graph to run queries on (default: cpg.bin.zip)")
        .action((x, c) => c.copy(cpgFilename = x))
      arg[String]("<query|filename>")
        .text("query to execute, or script file to execute if -f is set")
        .action((x, c) => c.copy(query = x))
      opt[Unit]('f', "isFile")
        .text("if specified, the second parameter is assumed to be a script file")
        .action((_, c) => c.copy(isFile = true))
    }.parse(args, Config("cpg.bin.zip", ""))

}
