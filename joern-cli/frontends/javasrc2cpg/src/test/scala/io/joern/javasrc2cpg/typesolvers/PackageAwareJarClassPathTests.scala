package io.joern.javasrc2cpg.typesolvers

import io.joern.javasrc2cpg.typesolvers.JarTypeSolver.*
import javassist.{ClassPool, CtClass}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}
import scala.compiletime.uninitialized

class PackageAwareJarClassPathTests extends AnyFreeSpec with Matchers with BeforeAndAfterAll {

  private var tempDir: Path               = uninitialized
  private var standardJarPath: Path       = uninitialized
  private var nonStandardJarPath: Path    = uninitialized
  private var mixedJarPath: Path          = uninitialized
  private var defaultPackageJarPath: Path = uninitialized

  override def beforeAll(): Unit = {
    tempDir = Files.createTempDirectory("jar-classpath-test")
    standardJarPath = createJarWithClasses(
      "standard.jar",
      classEntries = List(
        ClassEntry("com.example.Foo", "com/example/Foo.class"),
        ClassEntry("com.example.Bar", "com/example/Bar.class")
      )
    )
    nonStandardJarPath = createJarWithClasses(
      "nonstandard.jar",
      classEntries =
        List(ClassEntry("com.example.Foo", "lib/Foo.class"), ClassEntry("org.other.Bar", "repackaged/Bar.class"))
    )
    mixedJarPath = createJarWithClasses(
      "mixed.jar",
      classEntries = List(
        ClassEntry("com.example.Standard", "com/example/Standard.class"),
        ClassEntry("com.example.Misplaced", "wrong/path/Misplaced.class")
      )
    )
    defaultPackageJarPath =
      createJarWithClasses("defaultpkg.jar", classEntries = List(ClassEntry("RootClass", "RootClass.class")))
  }

  override def afterAll(): Unit = {
    Files.walk(tempDir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  "PackageAwareJarClassPath" - {
    "should resolve classes in a standard JAR by their actual names" in {
      val classPath = new PackageAwareJarClassPath(standardJarPath.toString)
      classPath.knownClassNames should contain allOf ("com.example.Foo", "com.example.Bar")
      classPath.openClassfile("com.example.Foo") should not be null
      classPath.openClassfile("com.example.Bar") should not be null
      classPath.find("com.example.Foo") should not be null
    }

    "should resolve classes at non-standard paths by their bytecode-declared package" in {
      val classPath = new PackageAwareJarClassPath(nonStandardJarPath.toString)
      classPath.knownClassNames should contain allOf ("com.example.Foo", "org.other.Bar")
      classPath.openClassfile("com.example.Foo") should not be null
      classPath.openClassfile("org.other.Bar") should not be null
      // Path-based names should NOT resolve
      classPath.openClassfile("lib.Foo") shouldBe null
      classPath.openClassfile("repackaged.Bar") shouldBe null
    }

    "should handle mixed JARs with both standard and non-standard paths" in {
      val classPath = new PackageAwareJarClassPath(mixedJarPath.toString)
      classPath.knownClassNames should contain allOf ("com.example.Standard", "com.example.Misplaced")
      classPath.openClassfile("com.example.Standard") should not be null
      classPath.openClassfile("com.example.Misplaced") should not be null
      classPath.openClassfile("wrong.path.Misplaced") shouldBe null
    }

    "should handle classes in the default package" in {
      val classPath = new PackageAwareJarClassPath(defaultPackageJarPath.toString)
      classPath.knownClassNames should contain("RootClass")
      classPath.openClassfile("RootClass") should not be null
    }

    "should return null for unknown classes" in {
      val classPath = new PackageAwareJarClassPath(standardJarPath.toString)
      classPath.openClassfile("does.not.Exist") shouldBe null
      classPath.find("does.not.Exist") shouldBe null
    }
  }

  "JarTypeSolver with non-standard JAR" - {
    "should register correct package prefixes from bytecode" in {
      val jarTypeSolver = new JarTypeSolverBuilder(enableVerboseTypeLogging = false)
        .withJars(Seq(nonStandardJarPath.toString))
        .build
      val combinedSolver = new SimpleCombinedTypeSolver(enableVerboseTypeLogging = false)
      combinedSolver.addNonCachingTypeSolver(jarTypeSolver)

      // The package prefix for com.example.Foo should be "com.example"
      // not "lib" (which would be derived from the path lib/Foo.class)
      val result = combinedSolver.tryToSolveType("com.example.Foo")
      result.isSolved shouldBe true
      result.getCorrespondingDeclaration.getQualifiedName shouldBe "com.example.Foo"

      val result2 = combinedSolver.tryToSolveType("org.other.Bar")
      result2.isSolved shouldBe true
      result2.getCorrespondingDeclaration.getQualifiedName shouldBe "org.other.Bar"
    }

    "should not resolve classes by their entry path names" in {
      val jarTypeSolver = new JarTypeSolverBuilder(enableVerboseTypeLogging = false)
        .withJars(Seq(nonStandardJarPath.toString))
        .build
      val combinedSolver = new SimpleCombinedTypeSolver(enableVerboseTypeLogging = false)
      combinedSolver.addNonCachingTypeSolver(jarTypeSolver)

      combinedSolver.tryToSolveType("lib.Foo").isSolved shouldBe false
      combinedSolver.tryToSolveType("repackaged.Bar").isSolved shouldBe false
    }
  }

  private case class ClassEntry(className: String, entryPath: String)

  /** Creates a JAR file containing class files at the specified entry paths, where each class file's bytecode declares
    * the given className regardless of entry path.
    */
  private def createJarWithClasses(jarName: String, classEntries: List[ClassEntry]): Path = {
    val jarPath = tempDir.resolve(jarName)
    val pool    = new ClassPool(true)

    val jos = new JarOutputStream(Files.newOutputStream(jarPath))
    try {
      classEntries.foreach { case ClassEntry(className, entryPath) =>
        val ctClass  = pool.makeClass(className)
        val bytecode = ctClass.toBytecode
        val jarEntry = new JarEntry(entryPath)
        jos.putNextEntry(jarEntry)
        jos.write(bytecode)
        jos.closeEntry()
        ctClass.detach()
      }
    } finally {
      jos.close()
    }

    jarPath
  }
}
