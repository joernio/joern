package io.shiftleft.cpgserver

import io.shiftleft.codepropertygraph.Cpg

class NullServerImpl extends ServerImpl {
  override def cpg: Option[Cpg] = None

  override def createCpg(filenames: List[String]): Unit = {}
}
