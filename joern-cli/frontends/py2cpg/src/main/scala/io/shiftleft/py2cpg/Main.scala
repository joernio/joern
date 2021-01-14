package io.shiftleft.py2cpg

object Main extends App {
  println("py2cpg")

  val py2CpgConfig = new Py2CpgConfig("outCpg", "/tmp/myPy")

  val py2cpg = new Py2Cpg(py2CpgConfig)
  py2cpg.buildCpg()
}
