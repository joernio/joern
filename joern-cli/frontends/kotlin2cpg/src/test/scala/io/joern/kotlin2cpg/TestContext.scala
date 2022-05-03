package io.joern.kotlin2cpg

import io.shiftleft.codepropertygraph.Cpg
import better.files._
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.utils.ProjectRoot

import scala.collection.mutable
import scala.util.Random

object TestContext {
  def newContext: TestContext = {
    new TestContext()
  }

  def buildCpg(
    code: String,
    file: String = "generated.kt",
    includeAllJars: Boolean = false,
    withTestResourceClassPath: Boolean = true
  ): Cpg = {
    val context = new TestContext()
    context.addSource(code, file)
    context.includeAllJars = includeAllJars
    context.withTestResourcePaths = withTestResourceClassPath
    context.buildCpg
  }
}

class TestContext private () {
  private val codeAndFile           = mutable.ArrayBuffer.empty[Kotlin2Cpg.InputPair]
  private var buildResult           = Option.empty[Cpg]
  private var includeAllJars        = false
  private var withTestResourcePaths = true

  def addSource(code: String, fileName: String = "generated.kt"): TestContext = {
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
      val dir           = File(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/jars/"))
      val config = Config(
        inputPaths = Set(tempDir.pathAsString),
        outputPath = randomOutPath,
        classpath = if (withTestResourcePaths) Set(dir.path.toAbsolutePath.toString) else Set(),
        withAndroidJarsInClassPath = includeAllJars
      )

      val kt2Cpg = new Kotlin2Cpg()
      val cpg    = kt2Cpg.createCpg(config)
      applyDefaultOverlays(cpg.get)
      val outFile = File(randomOutPath)
      if (outFile.exists) {
        outFile.delete(true)
      }
      buildResult = Some(cpg.get)
    }
    buildResult.get
  }
}
