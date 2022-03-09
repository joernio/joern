package io.joern.kotlin2cpg.integration

import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import io.joern.kotlin2cpg.{InferenceJarPath, Kt2Cpg, KtFileWithMeta}
import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultTypeInfoProvider, InferenceSourcesPicker}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.kotlin2cpg.files.SourceFilesPicker
import io.joern.kotlin2cpg.utils.PathUtils
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Files
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import scala.jdk.CollectionConverters.CollectionHasAsScala

class BasicAndroidAppValidationTests extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  var cpg: Cpg    = null
  val outFilePath = Files.createTempFile("kt2cpg-basic-android-app-test", ".bin.zip")

  override def beforeAll() = {
    super.beforeAll()
    val sourceDir =
      ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/code/barebone_android_app")
    cpg = makeCpg(sourceDir, outFilePath.toString)
  }

  override def afterAll() = {
    super.afterAll()
    cpg.close()
    Files.deleteIfExists(outFilePath)
  }

  def makeCpg(inPath: String, outPath: String): Cpg = {
    val jars            = ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/main/resources/inferencejars/")
    val inferenceJarDir = File(jars)
    val inferenceJarsPaths =
      inferenceJarDir.list
        .filter(_.extension.exists { e => e == ".jar" })
        .map { f =>
          InferenceJarPath(f.pathAsString, false)
        }
        .toSeq

    val dirsForSourcesToCompile = InferenceSourcesPicker.dirsForRoot(inPath)
    val environment =
      CompilerAPI.makeEnvironment(dirsForSourcesToCompile, inferenceJarsPaths, Seq(), new ErrorLoggingMessageCollector)
    val ktFiles = environment.getSourceFiles.asScala
    val filesWithMeta =
      ktFiles
        .flatMap { f =>
          try {
            val relPath = PathUtils.relativize(inPath, f.getVirtualFilePath)
            Some(f, relPath)
          } catch {
            case _: Throwable => None
          }
        }
        .map { fwp =>
          KtFileWithMeta(fwp._1, fwp._2, fwp._1.getVirtualFilePath)
        }
        .filterNot { fwp =>
          // TODO: add test for this type of filtering
          // TODO: support Windows paths
          val willFilter = SourceFilesPicker.shouldFilter(fwp.relativizedPath)
          if (willFilter) {
            println("Filtered file at `" + fwp.f.getVirtualFilePath + "`.")
          }
          willFilter
        }

    val nameGenerator = new DefaultTypeInfoProvider(environment)
    new Kt2Cpg().createCpg(filesWithMeta, Seq(), nameGenerator, Some(outPath))
  }

  "CPG for basic Android app" - {
    "should contain IDENTIFIER nodes for `webview` identifiers with the correct TYPE_FULL_NAME prop" in {
      cpg.identifier
        .code("webView")
        .typeFullName
        .toSet shouldBe Set("android.webkit.WebView")
    }
  }
}
