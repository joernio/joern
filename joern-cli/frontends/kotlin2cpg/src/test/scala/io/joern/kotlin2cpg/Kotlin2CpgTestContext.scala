package io.joern.kotlin2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.ProjectRoot
import better.files._
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.Random

object Kotlin2CpgTestContext {
  def newContext: Kotlin2CpgTestContext = {
    new Kotlin2CpgTestContext()
  }

  def buildCpg(code: String, file: String = "generated.kt", includeAllJars: Boolean = false): Cpg = {
    val context = new Kotlin2CpgTestContext()
    context.addSource(code, file)
    context.includeAllJars = includeAllJars
    context.buildCpg
  }
}

class Kotlin2CpgTestContext private () {
  private val codeAndFile    = mutable.ArrayBuffer.empty[Kotlin2Cpg.InputPair]
  private var buildResult    = Option.empty[Cpg]
  private var includeAllJars = false

  def addSource(code: String, fileName: String = "generated.kt"): Kotlin2CpgTestContext = {
    if (buildResult.nonEmpty) {
      throw new RuntimeException("Not allowed to add sources after buildCpg() was called.")
    }
    if (codeAndFile.exists(_.fileName == fileName)) {
      throw new RuntimeException(s"Add more than one source under file path $fileName.")
    }
    codeAndFile.append(Kotlin2Cpg.InputPair(code, fileName))
    this
  }

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val tempDir = File.newTemporaryDirectory().deleteOnExit(true)
      codeAndFile.foreach { inputPair =>
        val file = tempDir / inputPair.fileName
        file.writeText(inputPair.content)
      }
      val randomOutPath = "cpg_" + Random.alphanumeric.take(10).mkString + ".bin.zip"
      val path          = Paths.get("/tmp/" + randomOutPath)
      val config = Config(
        inputPaths = Set(tempDir.pathAsString),
        outputPath = path.toAbsolutePath.toString,
        classpath = Set(),
        withAndroidJarsInClassPath = includeAllJars,
        withMiscJarsInClassPath = includeAllJars
      )

      val kt2Cpg = new Kotlin2Cpg()
      val cpg    = kt2Cpg.createCpg(config)

      val context = new LayerCreatorContext(cpg.get)
      new Base().run(context)
      new TypeRelations().run(context)
      new ControlFlow().run(context)
      new CallGraph().run(context)

      val outFile = File(path)
      if (outFile.exists) {
        outFile.delete(true)
      }
      buildResult = Some(cpg.get)
    }
    buildResult.get
  }
}
