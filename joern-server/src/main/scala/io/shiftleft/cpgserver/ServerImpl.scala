package io.shiftleft.cpgserver

import io.shiftleft.codepropertygraph.Cpg

trait ServerImpl {

  def cpg: Option[Cpg]
  def createCpg(filenames: List[String]): Unit

}
