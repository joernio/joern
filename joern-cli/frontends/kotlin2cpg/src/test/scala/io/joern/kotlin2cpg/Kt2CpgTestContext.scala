package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultNameGenerator}
import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot
import better.files._
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Kt2CpgTestContext {
  def newContext: Kt2CpgTestContext = {
    new Kt2CpgTestContext()
  }

  /** This is a shorthand for newContext.addSource().buildCpg
    */
  def buildCpg(code: String, file: String = "test.kt"): Cpg = {
    val context = new Kt2CpgTestContext()
    context.addSource(code, file)
    context.buildCpg
  }
}

class Kt2CpgTestContext private () {
  private val codeAndFile = mutable.ArrayBuffer.empty[Kt2Cpg.InputPair]
  private var buildResult = Option.empty[Cpg]

  def addSource(code: String, fileName: String = "test.kt"): Kt2CpgTestContext = {
    if (buildResult.nonEmpty) {
      throw new RuntimeException("Not allowed to add sources after buildCpg() was called.")
    }
    if (codeAndFile.exists(_.fileName == fileName)) {
      throw new RuntimeException(s"Add more than one source under file path $fileName.")
    }
    codeAndFile.append(Kt2Cpg.InputPair(code, fileName))
    this
  }

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val tempDir = File.newTemporaryDirectory().deleteOnExit(true)
      codeAndFile.foreach { inputPair =>
        val file = tempDir / inputPair.fileName
        file.writeText(inputPair.content)
      }

      // TODO: iterate over inferencejars dir and get the paths like so

      val inferenceJarDir = File(
        ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/main/resources/inferencejars/")
      )
      val inferenceJarsPaths =
        inferenceJarDir.list
          .filter(_.hasExtension)
          .filter(_.pathAsString.endsWith("jar"))
          .map { f =>
            InferenceJarPath(f.pathAsString, false)
          }
          .toSeq

      val environment = CompilerAPI.makeEnvironment(
        Seq(tempDir.pathAsString),
        inferenceJarsPaths,
        Seq(),
        new ErrorLoggingMessageCollector
      )
      val filesWithMeta =
        environment.getSourceFiles.asScala
          .map { fm =>
            KtFileWithMeta(fm, "GENERATED_PLACEHOLDER_FILE.kt", fm.getVirtualFilePath)
          }

      val nameGenerator = new DefaultNameGenerator(environment)
      val kt2Cpg        = new Kt2Cpg()

      val nonSourceFiles = codeAndFile.map { entry =>
        FileContentAtPath(entry.content, entry.fileName, entry.fileName)
      }
      val cpg     = kt2Cpg.createCpg(filesWithMeta, nonSourceFiles, nameGenerator)
      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      new TypeRelations().run(context)
      new ControlFlow().run(context)
      new CallGraph().run(context)
      buildResult = Some(cpg)
    }
    buildResult.get
  }
}
