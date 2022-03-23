package io.joern.kotlin2cpg.integration

import io.joern.kotlin2cpg.{Config, InferenceJarPath, Kotlin2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Files
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll

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
    val kt2cpg = new Kotlin2Cpg()
    val config = Config(inputPaths = Set(inPath))
    val cpg    = kt2cpg.createCpg(config)

    cpg.get
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
