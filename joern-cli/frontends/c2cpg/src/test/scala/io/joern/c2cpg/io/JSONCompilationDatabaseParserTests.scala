package io.joern.c2cpg.io

import better.files.File as BetterFile
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.C2Cpg
import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*

import java.nio.file.{Files, Paths, Path}

class JSONCompilationDatabaseParserTests extends AnyWordSpec with Matchers {

  private def newProjectUnderTest(): Path = {
    val dir = Files.createTempDirectory("c2cpgJSONCompilationDatabaseParserTests")
    val mainText =
      """
        |int main(int argc, char *argv[]) {
        |  print("Hello World!");
        |}
        |#ifdef SOMEDEFA
        |void foo() {}
        |#endif
        |#ifdef SOMEDEFC
        |void bar() {}
        |#endif
        |""".stripMargin

    val fileA = dir / "fileA.c"
    fileA.createWithParentsIfNotExists(createParents = true)
    Files.writeString(fileA, mainText)
    FileUtil.deleteOnExit(fileA)

    val fileB = dir / "fileB.c"
    fileB.createWithParentsIfNotExists(createParents = true)
    Files.writeString(fileB, mainText)
    FileUtil.deleteOnExit(fileB)

    val fileC = dir / "fileC.c"
    fileC.createWithParentsIfNotExists(createParents = true)
    Files.writeString(fileC, mainText)
    FileUtil.deleteOnExit(fileC)

    val compilerCommands = dir / "compile_commands.json"
    compilerCommands.createWithParentsIfNotExists(createParents = true)
    val content =
      s"""
         |[
         |  { "directory": "${dir.toString}",
         |    "arguments": ["/usr/bin/clang++", "-Irelative", "-DSOMEDEFA=With spaces, quotes and \\-es.", "-c", "-o", "fileA.o", "fileA.cc"],
         |    "file": "fileA.c" },
         |  { "directory": ".",
         |    "arguments": ["/usr/bin/clang++", "-Irelative", "-DSOMEDEFB=With spaces, quotes and \\-es.", "-c", "-o", "fileB.o", "fileB.cc"],
         |    "file": "${fileB.toString}" }
         |]""".stripMargin.replace("\\", "\\\\") // escape for tests under Windows

    Files.writeString(compilerCommands, content)
    FileUtil.deleteOnExit(compilerCommands)
    FileUtil.deleteOnExit(dir)
    dir
  }

  private def newBrokenProjectUnderTest(): Path = {
    val dir = Files.createTempDirectory("c2cpgJSONCompilationDatabaseParserTests")

    val mainText =
      """
        |int main(int argc, char *argv[]) {
        |  print("Hello World!");
        |}
        |""".stripMargin

    val fileA = dir / "fileA.c"
    fileA.createWithParentsIfNotExists(createParents = true)
    Files.writeString(fileA, mainText)
    FileUtil.deleteOnExit(fileA)

    val compilerCommands = dir / "compile_commands.json"
    compilerCommands.createWithParentsIfNotExists(createParents = true)
    val content =
      s"""
         |[
         |  { "directory": "${dir.toString}",
         |    "arguments": ["/usr/bin/clang++", "-Irelative", "-DSOMEDEFA=With spaces, quotes and \\-es.", "-c", "-o", "fileA.o", "fileA.cc"],
         |    "file": "fileA.c" },
         |  { "directory": "/does/not/exist",
         |    "arguments": ["/usr/bin/clang++", "-c", "-o", "fileB.o", "name.cpp"],
         |    "file": "name.cpp" }
         |]""".stripMargin.replace("\\", "\\\\") // escape for tests under Windows
    Files.writeString(compilerCommands, content)
    FileUtil.deleteOnExit(compilerCommands)

    FileUtil.deleteOnExit(dir)
    dir
  }

  "Parsing a simple compile_commands.json" should {
    "generate a proper list of CommandObjects" in {
      val content =
        """
          |[
          |  { "directory": "/home/user/llvm/build",
          |    "arguments": ["/usr/bin/clang++", "-I/usr/include", "-I./include", "-DSOMEDEFA=With spaces, quotes and \\-es.", "-c", "-o", "file.o", "file.cc"],
          |    "file": "file.cc" },
          |  { "directory": "/home/user/llvm/build",
          |    "command": "/usr/bin/clang++ -I/home/user/project/includes -DSOMEDEFB=\"With spaces, quotes and \\-es.\" -DSOMEDEFC -c -o file.o file.cc",
          |    "file": "file2.cc" }
          |]""".stripMargin

      BetterFile.usingTemporaryFile("compile_commands.json") { commandJsonFile =>
        commandJsonFile.writeText(content)

        val commandObjects = JSONCompilationDatabaseParser.parse(commandJsonFile.pathAsString)
        commandObjects.map(_.compiledFile()) shouldBe Set(
          Paths.get("/home/user/llvm/build/file.cc").toString,
          Paths.get("/home/user/llvm/build/file2.cc").toString
        )
        commandObjects.flatMap(_.defines()) shouldBe Set(
          ("SOMEDEFA", "With spaces, quotes and \\-es."),
          ("SOMEDEFB", "\"With spaces, quotes and \\-es.\""),
          ("SOMEDEFC", "")
        )
        commandObjects.flatMap(_.includes()) shouldBe Set("/usr/include", "./include", "/home/user/project/includes")
      }
    }
  }

  "Using a simple compile_commands.json" should {
    "respect the files listed" in {
      val cpgOutFile = FileUtil.newTemporaryFile("c2cpg.bin")
      FileUtil.deleteOnExit(cpgOutFile)
      val projectUnderTest = newProjectUnderTest()
      val input            = projectUnderTest.toAbsolutePath.toString
      val output           = cpgOutFile.toString
      val config = Config()
        .withInputPath(input)
        .withOutputPath(output)
        .withCompilationDatabase((Paths.get(input) / "compile_commands.json").toString)
      val c2cpg = new C2Cpg()
      val cpg   = c2cpg.createCpg(config).get
      cpg.file.nameNot(FileTraversal.UNKNOWN, "<includes>").name.sorted.l should contain theSameElementsAs List(
        "fileA.c",
        "fileB.c"
        // fileC.c is ignored because it is not listed in the compile_commands.json
      )
      cpg.method.nameNot("<global>").name.sorted.l shouldBe List("foo", "main", "main")
    }

    "handle broken file paths" in {
      val cpgOutFile = FileUtil.newTemporaryFile("c2cpg.bin")
      FileUtil.deleteOnExit(cpgOutFile)
      val projectUnderTest = newBrokenProjectUnderTest()
      val input            = projectUnderTest.toAbsolutePath.toString
      val output           = cpgOutFile.toString
      val config = Config()
        .withInputPath(input)
        .withOutputPath(output)
        .withCompilationDatabase((Paths.get(input) / "compile_commands.json").toString)
      val c2cpg = new C2Cpg()
      val cpg   = c2cpg.createCpg(config).get
      cpg.file.nameNot(FileTraversal.UNKNOWN, "<includes>").name.l shouldBe List("fileA.c")
      cpg.method.nameNot("<global>").name.l shouldBe List("main")
    }
  }
}
