package io.shiftleft.joern.server

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.cpgserver.ServerImpl
import io.shiftleft.joern.{CpgLoader, JoernParse}
import org.slf4j.LoggerFactory

class JoernServerImpl extends ServerImpl {

  val logger = LoggerFactory.getLogger(getClass)

  var cpg: Option[Cpg] = None

  def createCpg(filenames: List[String]): Unit = {
    val cpgFilename = "/tmp/cpg.bin.zip"
    logger.info(s"Attempting to create CPG for: ${filenames.mkString(",")}")
    JoernParse.parse(filenames.toArray, cpgFilename)
    cpg = Some(CpgLoader.load(cpgFilename))
    logger.info("CPG is ready")
  }

}
