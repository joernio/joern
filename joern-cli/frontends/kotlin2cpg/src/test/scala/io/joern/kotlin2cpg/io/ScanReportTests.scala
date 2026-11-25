package io.joern.kotlin2cpg.io

import io.joern.kotlin2cpg.compiler.{CompilerAPI, ErrorLoggingMessageCollector}
import io.joern.kotlin2cpg.{Config, DefaultContentRootJarPath, Kotlin2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class ScanReportTests extends AnyWordSpec with Matchers with LoneElement {

  "kotlin2cpg" should {

    "report a classpath without jars as a problem" in {
      FileUtil.usingTemporaryDirectory("kotlin2cpgScanReport") { dir =>
        val src = dir / "src"
        Files.createDirectories(src)
        Files.writeString(src / "Main.kt", "fun main() { println(1) }")
        val emptyClasspath = Files.createDirectories(dir / "classpath")

        val report = new ScanReport()
        val config = Config().withInputPath(src.toString).withClasspath(Set(emptyClasspath.toString))
        new Kotlin2Cpg().createCpg(config, report).get.close()

        val problem = report.allProblems.find(_.message.contains("No jars found")).getOrElse(fail("no problem"))
        problem.severity shouldBe Severity.Warning
        problem.category shouldBe Category.DependencyResolution

        report.fileEntries.map(entry => (entry.path, entry.loc, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq(("Main.kt", Some(1), true, true))
      }
    }

    "report a dependency that does not exist as a problem" in {
      FileUtil.usingTemporaryDirectory("kotlin2cpgScanReport") { dir =>
        val missingJar  = (dir / "missing.jar").toString
        val report      = new ScanReport()
        val environment = CompilerAPI.makeEnvironment(
          Seq.empty,
          Seq.empty,
          Seq(DefaultContentRootJarPath(missingJar, isResource = false)),
          new ErrorLoggingMessageCollector,
          report
        )
        Disposer.dispose(environment.getProjectEnvironment.getParentDisposable)

        val problem = report.allProblems.loneElement
        problem.category shouldBe Category.DependencyResolution
        problem.file shouldBe Some(missingJar)
      }
    }
  }
}
