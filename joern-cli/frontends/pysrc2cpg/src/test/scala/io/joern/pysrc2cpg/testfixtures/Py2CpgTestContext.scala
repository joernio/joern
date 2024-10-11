package io.joern.pysrc2cpg.testfixtures

import io.joern.pysrc2cpg.Py2Cpg
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.defaultOverlayCreators
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

object Py2CpgTestContext {
  def buildCpg(code: String, file: String = "test.py"): Cpg = {
    val context = new Py2CpgTestContext()
    context.addSource(code, file)
    context.buildCpg
  }
}

class Py2CpgTestContext {

  private val codeAndFile     = mutable.ArrayBuffer.empty[Py2Cpg.InputPair]
  private var buildResult     = Option.empty[Cpg]
  private val absTestFilePath = "<absoluteTestPath>/"

  def addSource(code: String, file: String = "test.py"): Py2CpgTestContext = {
    if (buildResult.nonEmpty) {
      throw new RuntimeException("Not allowed to add sources after buildCpg() was called.")
    }
    if (codeAndFile.exists(_.relFileName == file)) {
      throw new RuntimeException(s"Add more than one source under file name $file.")
    }
    codeAndFile.append(Py2Cpg.InputPair(code, file))
    this
  }

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val cpg = new Cpg()
      val py2Cpg =
        new Py2Cpg(
          codeAndFile.map(inputPair => () => inputPair),
          cpg,
          absTestFilePath,
          schemaValidationMode = ValidationMode.Enabled,
          enableFileContent = true
        )
      py2Cpg.buildCpg()

      val context = new LayerCreatorContext(cpg)
      defaultOverlayCreators().foreach(_.run(context))
      buildResult = Some(cpg)
    }
    buildResult.get
  }
}
