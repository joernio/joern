package io.shiftleft.joern

import javax.script.ScriptEngineManager

object JoernQuery extends App {

  parseConfig.foreach {config =>
    val e = new ScriptEngineManager().getEngineByName("scala");
    val script =
      s"""
        | import io.shiftleft.joern.CpgLoader
        | import io.shiftleft.queryprimitives.steps.starters.Cpg
        | val cpg : Cpg = CpgLoader.load("${config.cpgFilename}")
        | val result = ${config.query}
        | result.toString
      """.stripMargin

    println(e.eval(script));
  }

  case class Config(cpgFilename : String, query : String)
  def parseConfig : Option[Config] = new scopt.OptionParser[Config]("joern-query") {
    arg[String]("<cpg>").action((x, c) => c.copy(cpgFilename = x))
    arg[String]("<query>").action((x, c) => c.copy(query = x))
  }.parse(args, Config("", ""))

}
