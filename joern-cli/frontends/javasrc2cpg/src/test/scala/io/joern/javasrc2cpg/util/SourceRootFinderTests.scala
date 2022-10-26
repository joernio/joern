package io.joern.javasrc2cpg.util

import better.files.File
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

class SourceRootFinderTests extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val rootTmpDir: File = File.newTemporaryDirectory("javasrc_root_test").deleteOnExit()
  private lazy val rootPath    = rootTmpDir.pathAsString
  // Root path using unix separators
  private lazy val stdRootPath = rootPath.replaceAll("\\" ++ java.io.File.separator, "/")

  override def beforeAll(): Unit = {

    val directoriesToCreate = List(
      File(rootPath, "maven", "mvn1", "src", "main", "java", "io"),
      File(rootPath, "maven", "mvn1", "src", "main", "scala", "io"),
      File(rootPath, "maven", "mvn1", "src", "test", "java", "io"),
      File(rootPath, "maven", "mvn1", "src", "main", "resources", "jars"),
      File(rootPath, "maven", "mvn2", "src", "main", "java", "io"),
      File(rootPath, "src", "io"),
      File(rootPath, "test", "io"),
      File(rootPath, "some", "nested", "directories", "src", "io"),
      File(rootPath, "some", "nested", "more", "directories", "src", "io"),
      File(rootPath, "some", "nested", "more", "directories", "test", "io"),
      File(rootPath, "nosrc", "nested", "directories", "io"),
      File(rootPath, "code", "project", "src", "main"),
      File(rootPath, "code", "project", "src", "random"),
      File(rootPath, "code", "project", "src", "test")
    )

    directoriesToCreate.foreach { file =>
      file.createDirectories()
      file.deleteOnExit()
    }
  }

  override def afterAll(): Unit = {
    super.afterAll()
    if (rootTmpDir.exists) {
      rootTmpDir.delete()
    }
  }

  private def rootsWithUnixSeparators(file: File): List[String] = {
    SourceRootFinder
      .getSourceRoots(file.pathAsString)
      .map { srcPath => srcPath.replaceAll("\\" ++ java.io.File.separator, "/") }
  }

  it should "find all the correct source directories if tmp root is given" in {
    val sourceRoots = rootsWithUnixSeparators(File(rootPath))
    sourceRoots.sorted shouldBe List(
      s"$stdRootPath/maven/mvn1/src/main/java",
      s"$stdRootPath/maven/mvn2/src/main/java",
      // Over-approximate to avoid missing sources
      s"$stdRootPath/maven/mvn1/src/main/resources",
      s"$stdRootPath/maven/mvn1/src/main/scala",
      s"$stdRootPath/src",
      s"$stdRootPath/some/nested/directories/src",
      s"$stdRootPath/some/nested/more/directories/src",
      s"$stdRootPath/code/project/src/main",
      s"$stdRootPath/code/project/src/random"
    ).sorted
  }

  it should "find the given directory if no matching subdirectories are found" in {
    val sourceRoots = rootsWithUnixSeparators(File(rootPath, "nosrc"))
    sourceRoots shouldBe List(s"$stdRootPath/nosrc")
  }

  it should "find a src directory without main/test subdirectory" in {
    val sourceRoots = rootsWithUnixSeparators(File(rootPath, "some", "nested", "directories"))
    sourceRoots shouldBe List(s"$stdRootPath/some/nested/directories/src")

    val specificSourceRoots = rootsWithUnixSeparators(File(rootPath, "some", "nested", "directories", "src"))
    specificSourceRoots shouldBe List(s"$stdRootPath/some/nested/directories/src")
  }

  it should "find the correct directory if a rootPath partly into a src/maintest/java string is given" in {
    val srcRoot = rootsWithUnixSeparators(File(rootPath, "maven", "mvn2", "src"))
    srcRoot shouldBe List(s"$stdRootPath/maven/mvn2/src/main/java")

    val mainRoot = rootsWithUnixSeparators(File(rootPath, "maven", "mvn2", "src", "main"))
    mainRoot shouldBe List(s"$stdRootPath/maven/mvn2/src/main/java")

    val javaRoot = rootsWithUnixSeparators(File(rootPath, "maven", "mvn2", "src", "main", "java"))
    javaRoot shouldBe List(s"$stdRootPath/maven/mvn2/src/main/java")

    // This is an example of where the SourceRootFinder gets the wrong result. This is because it never
    // searches "up" from the given directory.
    val ioRoot = rootsWithUnixSeparators(File(rootPath, "maven", "mvn2", "src", "main", "java", "io"))
    ioRoot shouldBe List(s"$stdRootPath/maven/mvn2/src/main/java/io")
  }
}
