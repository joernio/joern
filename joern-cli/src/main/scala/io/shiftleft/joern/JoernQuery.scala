package io.shiftleft.joern

import java.io.FileReader
import javax.script.ScriptEngineManager

object JoernQuery extends App {

  parseConfig.foreach { config =>
    val e = new ScriptEngineManager().getEngineByName("scala")

    val cpgLoadingCode =
      s"""
      | import io.shiftleft.joern.CpgLoader
      | import io.shiftleft.codepropertygraph.Cpg
      | val cpg : Cpg = CpgLoader.load("${config.cpgFilename}")
      |""".stripMargin

    val context = e.getContext
    e.eval(cpgLoadingCode, context)

    if (config.isFile) {
      val reader = new FileReader(config.query)
      println(e.eval(reader, context))
    } else {
      val script = config.query + ".l.mkString(\"\\n\")"
      println(e.eval(script, context))
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
