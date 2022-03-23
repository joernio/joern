package io.joern.javasrc2cpg.util

import better.files.File
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SourceRootFinderTests extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val rootTmpDir: File = File.newTemporaryDirectory("javasrc_root_test").deleteOnExit()
  private lazy val rootPath    = rootTmpDir.pathAsString

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
      File(rootPath, "nosrc", "nested", "directories", "io")
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

  it should "find all the correct source directories if tmp root is given" in {
    val sourceRoots = SourceRootFinder.getSourceRoots(rootPath)
    sourceRoots.sorted shouldBe List(
      s"$rootPath/maven/mvn1/src/main/java",
      s"$rootPath/maven/mvn1/src/test/java",
      s"$rootPath/maven/mvn2/src/main/java",
      s"$rootPath/src",
      s"$rootPath/test",
      s"$rootPath/some/nested/directories/src",
      s"$rootPath/some/nested/more/directories/src",
      s"$rootPath/some/nested/more/directories/test"
    ).sorted
  }

  it should "find the given directory if no matching subdirectories are found" in {
    val sourceRoots = SourceRootFinder.getSourceRoots(s"$rootPath/nosrc")
    sourceRoots shouldBe List(s"$rootPath/nosrc")
  }

  it should "find a src directory without main/test subdirectory" in {
    val sourceRoots = SourceRootFinder.getSourceRoots(s"$rootPath/some/nested/directories")
    sourceRoots shouldBe List(s"$rootPath/some/nested/directories/src")

    val specificSourceRoots = SourceRootFinder.getSourceRoots(s"$rootPath/some/nested/directories/src")
    specificSourceRoots shouldBe List(s"$rootPath/some/nested/directories/src")
  }

  it should "find the correct directory if a rootPath partly into a src/maintest/java string is given" in {
    val srcRoot = SourceRootFinder.getSourceRoots(s"$rootPath/maven/mvn2/src")
    srcRoot shouldBe List(s"$rootPath/maven/mvn2/src/main/java")

    val mainRoot = SourceRootFinder.getSourceRoots(s"$rootPath/maven/mvn2/src/main")
    mainRoot shouldBe List(s"$rootPath/maven/mvn2/src/main/java")

    val javaRoot = SourceRootFinder.getSourceRoots(s"$rootPath/maven/mvn2/src/main/java")
    javaRoot shouldBe List(s"$rootPath/maven/mvn2/src/main/java")

    // This is an example of where the SourceRootFinder gets the wrong result. This is because it never
    // searches "up" from the given directory.
    val ioRoot = SourceRootFinder.getSourceRoots(s"$rootPath/maven/mvn2/src/main/java/io")
    ioRoot shouldBe List(s"$rootPath/maven/mvn2/src/main/java/io")
  }
}
