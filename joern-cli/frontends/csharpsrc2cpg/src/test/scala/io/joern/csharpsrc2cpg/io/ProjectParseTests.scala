package io.joern.csharpsrc2cpg.io

import better.files.File
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg
import io.joern.csharpsrc2cpg.utils.{AstGenRunner}
import org.scalatest.BeforeAndAfterAll

class ProjectParseTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private val projectWithSubfolders: File = {
    val dir = File.newTemporaryDirectory("csharpsrc2cpgTestsSubfolders")
    List("a.cs", "b.cs").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      file.write(s"""
           |using System;
           |
           |namespace HelloWorld
           |{
           |  class Program
           |  {
           |    static void Main(string[] args)
           |    {
           |      Console.WriteLine("File path:" + file.canonicalPath);
           |    }
           |  }
           |}""".stripMargin)
    }
    dir
  }

  override def afterAll(): Unit = {
    projectWithSubfolders.delete(swallowIOExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: File)(f: Array[String] => Unit): Unit = {
      File.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
        val cpg          = newEmptyCpg()
        val config       = Config().withInputPath(projectDir.toString).withOutputPath(tmpDir.toString)
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
//        TODO: Update the test case with AstCreationPass once that in place
//        new AstCreationPass(cpg, astGenResult, config).createAndApply()
//        f(cpg)
        f(astGenResult.parsedFiles.toArray)
      }
    }
  }

  "Parsing a project" should {
    "generate correct filenames" in ProjectParseTestsFixture(projectWithSubfolders) { (parsedFiles: Array[String]) =>
      println(parsedFiles)
      val fileNames = parsedFiles.map(filePath => {
        val lastSlashIndex = filePath.lastIndexOf(java.io.File.separator)
        val fileName       = if (lastSlashIndex >= 0) filePath.substring(lastSlashIndex + 1) else filePath
        fileName
      })
      fileNames.toList should contain allElementsOf List("a.json", "b.json")
    }

  }

}
