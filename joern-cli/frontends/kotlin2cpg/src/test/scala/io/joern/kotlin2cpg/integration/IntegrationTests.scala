package io.joern.kotlin2cpg.integration

import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import io.joern.kotlin2cpg.{InferenceJarPath, Kt2Cpg, KtFileWithMeta}
import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultNameGenerator, InferenceSourcesPicker}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.kotlin2cpg.files.SourceFilesPicker
import io.joern.kotlin2cpg.utils.PathUtils

import java.nio.file.Files
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Ignore
import overflowdb.traversal.jIteratortoTraversal

import scala.jdk.CollectionConverters.CollectionHasAsScala

@Ignore // re-enable with a good setup for cloning and syncing external projects
class IntegrationTests extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  var cpg: Cpg    = null
  val outFilePath = Files.createTempFile("kt2cpg-integration-test-artifact", ".bin.zip")

  override def beforeAll() = {
    super.beforeAll()
    val sourceDir = "src/test/resources/external_projects/AceJump"
    cpg = makeCpg(sourceDir, outFilePath.toString)
  }

  override def afterAll() = {
    super.afterAll()
    cpg.close()
    Files.deleteIfExists(outFilePath)
  }

  def makeCpg(inPath: String, outPath: String): Cpg = {
    val inferenceJarDir = File("joern-cli/frontends/kotlin2cpg/src/main/resources/inferencejars/")
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

    val nameGenerator = new DefaultNameGenerator(environment)
    new Kt2Cpg().createCpg(filesWithMeta, Seq(), nameGenerator, Some(outPath))
  }

  "CPG generated from large sample project" - {
    "should not contain any METHOD nodes with FNs starting with the char `.`" in {
      cpg.method.fullName("\\..*").fullName.l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs starting with the char `.`" in {
      cpg.call.methodFullName("\\..*").methodFullName.l shouldBe List()
    }

    "should not contain the substring `ERROR` in any MFNs of CALL nodes" in {
      cpg.call.methodFullName(".*ERROR.*").methodFullName.l shouldBe List()
    }

    "should not contain the substring `ERROR` in any FNs of METHOD nodes" in {
      cpg.method.fullName(".*ERROR.*").fullName.l shouldBe List()
    }

    "should not contain the substring `?` in any FNs of METHOD nodes" in {
      cpg.method.fullName(".*\\?.*").fullName.l shouldBe List()
    }

    "should not contain the substring `?` in any MFNs of CALL nodes" in {
      cpg.call.methodFullName(".*\\?.*").methodFullName.l shouldBe List()
    }

    "should not contain the `{` char in any FNs of METHOD nodes" in {
      cpg.method.filter(_.fullName.contains('{')).fullName.l shouldBe List()
    }

    "should not contain the `{` char in any MFNs of CALL nodes" in {
      cpg.call.filter(_.methodFullName.contains('{')).methodFullName.l shouldBe List()
    }

    "should not contain any METHOD nodes with FNs starting with `.`" in {
      cpg.method.fullName("\\..*").fullName.l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs starting with `.`" in {
      cpg.call.methodFullName("\\..*").methodFullName.l shouldBe List()
    }

    "should not contain any METHOD nodes with FNs with a space character in them" in {
      cpg.method.fullName(".*[ ].*").fullName.l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a space character in them" in {
      cpg.call.methodFullName(".*[ ].*").methodFullName.l shouldBe List()
    }

    "should not contain any METHOD nodes with FNs with a the `>` character in them" in {
      cpg.method
        .fullNameNot(".*<lambda>.*")
        .fullNameNot(".*<init>.*")
        .fullNameNot("<operator>.*")
        .fullName(".*>.*")
        .fullName
        .l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a the `>` character in them" in {
      cpg.call
        .methodFullNameNot(".*<lambda>.*")
        .methodFullNameNot(".*<init>.*")
        .methodFullNameNot("<operator>.*")
        .methodFullName(".*>.*")
        .methodFullName
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
}
