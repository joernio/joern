package io.joern.javasrc2cpg.typesolvers

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.util.{Success, Using}

class JrtRuntimeImageClassPathTests extends AnyFreeSpec with Matchers {

  private def javaHome = Paths.get(System.getProperty("java.home"))

  private def assumeRuntimeImage(): Unit = {
    assume(
      JrtRuntimeImageClassPath.findRuntimeImageRoot(javaHome).isDefined,
      s"Skipping: no lib/modules at $javaHome (not a modular JDK layout)"
    )
  }

  "findRuntimeImageRoot" - {

    "should find lib/modules nested under the search root (single walk returns java.home and modules path)" in {
      FileUtil.usingTemporaryDirectory("jrt-nested-modules") { tmp =>
        val imageRoot   = tmp.resolve("vendor").resolve("my-jlink")
        val modulesFile = imageRoot.resolve("lib").resolve("modules")
        Files.createDirectories(imageRoot.resolve("lib"))
        Files.createFile(modulesFile)
        val layout = JrtRuntimeImageClassPath.findRuntimeImage(tmp).get
        layout.javaHome shouldEqual imageRoot
        layout.modulesImageFile shouldEqual modulesFile
      }
    }

    "should prefer the shallowest runtime image when several exist" in {
      FileUtil.usingTemporaryDirectory("jrt-shallow-wins") { tmp =>
        val deep = tmp.resolve("a").resolve("b").resolve("c")
        Files.createDirectories(deep.resolve("lib"))
        Files.createFile(deep.resolve("lib").resolve("modules"))
        val shallow = tmp.resolve("z")
        Files.createDirectories(shallow.resolve("lib"))
        Files.createFile(shallow.resolve("lib").resolve("modules"))
        JrtRuntimeImageClassPath.findRuntimeImageRoot(tmp) shouldEqual Some(shallow)
      }
    }
  }

  "JrtRuntimeImageClassPath" - {

    "should index and open java.lang.String from the running JDK's runtime image" in {
      assumeRuntimeImage()
      val cp = new JrtRuntimeImageClassPath(javaHome)
      cp.knownClassNames should contain("java.lang.String")
      Using.resource(cp.openClassfile("java.lang.String")) { in =>
        in should not be null
        in.available() should be > 0
      }
      cp.find("java.lang.String") should not be null
    }

    "should expose module exports for java.base" in {
      assumeRuntimeImage()
      val cp = new JrtRuntimeImageClassPath(javaHome)
      cp.moduleExportsMap.get("java.base") should not be empty
      cp.moduleExportsMap("java.base") should contain("java.lang")
    }
  }

  "JarTypeSolver with runtime image only" - {

    "should resolve java.lang.String via tryToSolveTypeInModule" in {
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
}
