package io.joern.kotlin2cpg.jar4import

import org.slf4j.LoggerFactory

class Service(url: String) {
  private val logger = LoggerFactory.getLogger(getClass)

  def fetchDependencyCoordinates(imports: Seq[String]): Seq[String] = {
    try {
      val resp = requests.get(url + "/find", params = Map("names" -> imports.mkString(",")))
      if (resp.statusCode == 200) {
        val got = ujson.read(resp.bytes)
        got("matches") match {
          case arr: ujson.Arr =>
            val out = arr.value.collect { case s: ujson.Str => s.toString() }
            logger.debug(s"Found `${out.size}` matches for provided imports `$imports`.")
            out.toSeq
          case _ =>
            Seq()
        }
      } else Seq()
    } catch {
      case _: Throwable => Seq()
    }
  }
}
