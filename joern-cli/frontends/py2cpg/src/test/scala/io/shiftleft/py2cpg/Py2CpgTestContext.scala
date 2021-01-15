package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.{LayerCreatorContext, Scpg}
import scala.collection.mutable

object Py2CpgTestContext {
  def newContext: Py2CpgTestContext = {
    new Py2CpgTestContext()
  }

  /**
    * This is a shorthand for newContext.addSource().buildCpg
    */
  def buildCpg(code: String, file: String = "test.py"): Cpg = {
    val context = new Py2CpgTestContext()
    context.addSource(code, file)
    context.buildCpg
  }
}

class Py2CpgTestContext private () {
  private val codeAndFile = mutable.ArrayBuffer.empty[Py2Cpg.InputPair]
  private var buildResult = Option.empty[Cpg]

  def addSource(code: String, file: String = "test.py"): Py2CpgTestContext = {
    if (buildResult.nonEmpty) {
      throw new RuntimeException("Not allowed to add sources after buildCpg() was called.")
    }
    if (codeAndFile.exists(_.file == file)) {
      throw new RuntimeException(s"Add more than one source under file name $file.")
    }
    codeAndFile.append(new Py2Cpg.InputPair(code, file))
    this
  }

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val cpg = new Cpg()
      val py2Cpg = new Py2Cpg(
        codeAndFile.map(inputPair => () => inputPair),
        cpg
      )
      py2Cpg.buildCpg()

      val context = new LayerCreatorContext(cpg)
      new Scpg().run(context)
      buildResult = Some(cpg)
    }
    buildResult.get
  }
}
