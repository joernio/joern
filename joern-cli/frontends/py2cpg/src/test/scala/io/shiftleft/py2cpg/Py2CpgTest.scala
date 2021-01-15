package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.{LayerCreatorContext, Scpg}

object Py2CpgTest {
  def newContext: Py2CpgTestContext = {
    new Py2CpgTestContext(Nil)
  }
}

class Py2CpgTestContext(codeAndFile: List[Py2Cpg.InputPair]) {
  def buildCpg(code: String, file: String = "test.py"): Cpg = {
    new Py2CpgTestContext(new Py2Cpg.InputPair(code, file) :: codeAndFile).buildCpg
  }

  def addSource(code: String, file: String): Py2CpgTestContext = {
    new Py2CpgTestContext(new Py2Cpg.InputPair(code, file) :: codeAndFile)
  }

  def buildCpg: Cpg = {
    val cpg = new Cpg()
    val py2Cpg = new Py2Cpg(
      codeAndFile.map(inputPair => () => inputPair),
      cpg
    )
    py2Cpg.buildCpg()

    val context = new LayerCreatorContext(cpg)
    new Scpg().run(context)
    cpg
  }
}
