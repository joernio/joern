package io.joern.javasrc2cpg.typesolvers

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.util.{Success, Try, Using}

class JrtRuntimeImageClassPathTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private def javaHome = Paths.get(System.getProperty("java.home"))

  /** Shared instance — only constructed if a test actually needs it (lazy), and closed after all tests run. */
  private lazy val cp = new JrtRuntimeImageClassPath(javaHome)

  override def afterAll(): Unit = {
    Try(cp.close())
    super.afterAll()
  }

  private def assumeRuntimeImage(): Unit = {
    assume(
      JrtRuntimeImageClassPath.findRuntimeImage(javaHome).isDefined,
      s"Skipping: no lib/modules at $javaHome (not a modular JDK layout)"
    )
  }

  "findRuntimeImageRoot" should {

    "find lib/modules nested under the search root (single walk returns java.home and modules path)" in {
      FileUtil.usingTemporaryDirectory("jrt-nested-modules") { tmp =>
        val imageRoot   = tmp.resolve("vendor").resolve("my-jlink")
        val modulesFile = imageRoot.resolve("lib").resolve("modules")
        Files.createDirectories(imageRoot.resolve("lib"))
        Files.createFile(modulesFile)
        JrtRuntimeImageClassPath.findRuntimeImage(tmp) shouldEqual Some(imageRoot)
      }
    }
  }

  "JrtRuntimeImageClassPath" should {

    "index and open java.lang.String from the running JDK's runtime image" in {
      assumeRuntimeImage()
      cp.knownClassNames should contain("java.lang.String")
      Using.resource(cp.openClassfile("java.lang.String")) { in =>
        in should not be null
        in.available() should be > 0
      }
      cp.find("java.lang.String") should not be null
    }

    "expose module exports for java.base" in {
      assumeRuntimeImage()
      cp.moduleExportsMap.get("java.base") should not be empty
      cp.moduleExportsMap("java.base") should contain("java.lang")
    }
  }

  "JarTypeSolver with runtime image only" should {

    "resolve java.lang.String via tryToSolveTypeInModule" in {
      assumeRuntimeImage()
      val builder = new JarTypeSolverBuilder(enableVerboseTypeLogging = false)
      builder.addRuntimeImage(javaHome) shouldEqual Success(())
      val jarTypeSolver  = builder.build
      val combinedSolver = new SimpleCombinedTypeSolver(enableVerboseTypeLogging = false)
      combinedSolver.addNonCachingTypeSolver(jarTypeSolver)

      val r = combinedSolver.tryToSolveTypeInModule("java.base", "String")
      r.isSolved shouldBe true
      r.getCorrespondingDeclaration.getQualifiedName shouldBe "java.lang.String"
    }
  }

  "JarTypeSolver.fromPath" should {

    "throw IllegalArgumentException when no jars and no runtime image are found" in {
      FileUtil.usingTemporaryDirectory("jrt-no-jars-no-modules") { tmp =>
        val exception = the[IllegalArgumentException] thrownBy {
          JarTypeSolver.fromPath(tmp.toString)
        }
        exception.getMessage should include("No .jar or .jmod files found")
        exception.getMessage should include("no runtime image file")
      }
    }
  }
}
