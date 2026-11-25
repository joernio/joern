package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

/** Dependency problems must end up in the [[ScanReport]]. None of these tests needs a build tool or the network. */
class DependencyProblemsTests extends AnyWordSpec with Matchers with LoneElement {

  private def dependencyProblems(report: ScanReport) = {
    report.allProblems.map(problem => (problem.severity, problem.category)).distinct shouldBe
      Seq((Severity.Warning, Category.DependencyResolution))
    report.allProblems.map(_.message)
  }

  private def projectJson(name: String): String =
    s"""{"projectName": "$name", "sourceRoots": [], "sourceDependencies": [], "artifactDependencies": []}"""

  "DependencyResolverV2.resolve" should {
    "report a project without a supported build file" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        val report = new ScanReport()
        DependencyResolverV2.resolve(dir.toString, problems = report) shouldBe None
        dependencyProblems(report).loneElement should include("no supported build file found")
      }
    }

    "not require a report" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        DependencyResolverV2.resolve(dir.toString) shouldBe None
      }
    }
  }

  "DependencyGraph.fromJson" should {
    "report a dependency info file that cannot be parsed" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        val broken = dir / "broken.json"
        Files.writeString(broken, "{ this is not json")
        val report = new ScanReport()

        DependencyGraph.fromJson(dir, Array(broken), report).nodes shouldBe empty

        dependencyProblems(report).loneElement should include("attempting to parse dependency json")
        report.allProblems.loneElement.exception should not be empty
      }
    }

    "report duplicate project definitions" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        val first  = dir / "a.json"
        val second = dir / "b.json"
        Files.writeString(first, projectJson(":lib"))
        Files.writeString(second, projectJson(":lib"))
        val report = new ScanReport()

        DependencyGraph.fromJson(dir, Array(first, second), report).nodes.keys.toSeq shouldBe Seq(":lib")

        dependencyProblems(report).loneElement should include("duplicate project definition: :lib")
      }
    }

    "not report valid dependency info" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        val json = dir / "a.json"
        Files.writeString(json, projectJson(":lib"))
        val report = new ScanReport()
        DependencyGraph.fromJson(dir, Array(json), report)
        report.allProblems shouldBe empty
      }
    }
  }

  "AarExtractor" should {
    "report an aar that cannot be unzipped" in {
      FileUtil.usingTemporaryDirectory("dependencyProblems") { dir =>
        val aar: Path = dir / "broken.aar"
        Files.writeString(aar, "not a zip file")
        val report = new ScanReport()

        AarExtractor.materializeJars(Seq(aar), dir / "cache", report) shouldBe empty

        dependencyProblems(report).loneElement should include("Failed to unzip aar")
      }
    }
  }
}
