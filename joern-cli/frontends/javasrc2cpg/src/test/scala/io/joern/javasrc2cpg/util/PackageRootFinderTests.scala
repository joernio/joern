package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.testfixtures.SourceCodeFixture

import java.nio.file.Path

class PackageRootFinderTests extends SourceCodeFixture {
  def rootFinderTest(
    files: List[(String, String)],
    expectedRoots: Set[String],
    nonSourceFiles: List[(String, String)] = Nil
  ): Unit = {
    val testDir = (files ++ nonSourceFiles)
      .foldLeft(emptyWriter) { case (writer, (code, fileName)) => writer.moreCode(code, fileName) }
      .writeCode(".java")

    val absoluteFilenames = files.map { case (_, filename) => testDir.resolve(filename) }

    absoluteFilenames.permutations.foreach { filePermutation =>
      val inputs     = filePermutation.flatMap(input => SourceParser.FileInfo.getFileInfo(testDir, input.toString))
      val foundRoots = PackageRootFinder.packageRootsFromFiles(testDir, inputs).toSet

      if (foundRoots != expectedRoots.map(Path.of(_))) {
        fail(s"""Found package roots did not match expected package roots:
                |Found roots   : ${foundRoots.toList.sorted.mkString(",")}
                |Expected roots: ${expectedRoots.toList.sorted.mkString(",")}
                |for input path permutation ${inputs.map(_.relativePath).mkString(",")}""".stripMargin)
      }
    }
  }

  "the package root finder" should {
    "find the package root for correctly structured code with a single root" in {
      val files = List(
        ("package foo;", "src/main/java/foo/A.java"),
        ("package foo.bar;", "src/main/java/foo/bar/B.java"),
        ("package baz;", "src/main/java/baz/C.java")
      )

      rootFinderTest(files, Set("src/main/java"))
    }

    "find package roots for correctly structured code with multiple roots" in {
      val files = List(
        ("package foo;", "src/main/java/foo/A.java"),
        ("package foo.bar;", "src/main/java/foo/bar/B.java"),
        ("package baz;", "src/test/java/baz/C.java")
      )

      rootFinderTest(files, Set("src/main/java", "src/test/java"))
    }

    "find package roots for correctly structured code with multiple roots and non-source files" in {
      val files = List(
        ("package foo;", "src/main/java/foo/A.java"),
        ("package foo.bar;", "src/main/java/foo/bar/B.java"),
        ("package baz;", "src/test/java/baz/C.java")
      )

      val nonSourceFiles = List(("some stuff", "src/main/resources/stuff.config"))

      rootFinderTest(files, Set("src/main/java", "src/test/java"), nonSourceFiles)
    }

    "default to separate filenames for incorrectly structured code" in {
      val files = List(
        ("package unmatched;", "src/main/java/foo/A.java"),
        ("package unmatched.other;", "src/main/java/foo/bar/B.java"),
        ("package moreunmatched;", "src/test/java/baz/C.java")
      )

      rootFinderTest(files, files.map(_._2).toSet)
    }

    "find the correct paths for mixed correctly and incorrectly structured code" in {
      val files = List(
        ("package unmatched;", "src/main/java/foo/A.java"),
        ("package foo.bar;", "src/main/java/foo/bar/B.java"),
        ("package more.unmatched;", "src/test/java/baz/C.java"),
        ("package even.more.unmatched;", "other/src/main/java/baz/D.java")
      )

      rootFinderTest(
        files,
        // A.java and C.java are included twice in the found root hierarchy which is not ideal, but this is also
        // not a situation that will happen in sensible codebases.
        Set("src/main/java", "src/main/java/foo/A.java", "src/test/java/baz/C.java", "other/src/main/java/baz/D.java")
      )
    }

    "find the correct paths with only sources at the project root" in {
      val files = List(("", "A.java"), ("", "B.java"))
      rootFinderTest(files, Set("A.java", "B.java"))
    }

    "find the correct paths with some sources at the project root" in {
      val files = List(("", "A.java"), ("", "B.java"), ("package foo;", "src/foo/C.java"))
      rootFinderTest(files, Set("A.java", "B.java", "src"))
    }

  }
}
