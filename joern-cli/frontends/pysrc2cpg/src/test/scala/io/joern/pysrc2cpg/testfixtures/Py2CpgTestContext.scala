package io.joern.pysrc2cpg.testfixtures

import io.joern.pysrc2cpg.{Py2Cpg, Py2CpgOnFileSystemConfig}
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.defaultOverlayCreators
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

import scala.collection.mutable

object Py2CpgTestContext {
  def buildCpg(code: String, file: String = "test.py"): Cpg = {
    val context = new Py2CpgTestContext()
    context.addSource(code, file)
    context.buildCpg
  }

  def addSource(code: String, file: String = "test.py"): Py2CpgTestContext = {
    val context = new Py2CpgTestContext()
    context.addSource(code, file)
  }
}

class Py2CpgTestContext {

  private val codeAndFile       = mutable.ArrayBuffer.empty[Py2Cpg.InputPair]
  private var buildResult       = Option.empty[Cpg]
  private val absTestFilePath   = "absoluteTestPath/"
  private var enableFileContent = true

  def withEnabledFileContent(value: Boolean): Py2CpgTestContext = {
    enableFileContent = value
    this
  }

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
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    val config = Py2CpgOnFileSystemConfig()
      .withInputPath(absTestFilePath)
      .withSchemaValidation(ValidationMode.Enabled)
      .withDisableFileContent(!enableFileContent)
    if (buildResult.isEmpty) {
      val cpg = new Cpg()
      val py2Cpg =
        new Py2Cpg(codeAndFile.map(inputPair => () => inputPair), cpg, config)
      py2Cpg.buildCpg()

      val context = new LayerCreatorContext(cpg)
      defaultOverlayCreators().foreach(_.run(context))
      buildResult = Some(cpg)
    }
    buildResult.get
  }
}
