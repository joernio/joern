package io.joern.csharpsrc2cpg.io

import better.files.File
import io.joern.csharpsrc2cpg.passes.AstCreationPass
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.BeforeAndAfterAll
import io.shiftleft.semanticcpg.language.*

class ProjectParseTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private val sep = java.io.File.separator

  private val projectWithSubfolders: File = {
    val dir = File.newTemporaryDirectory("csharpsrc2cpgTestsSubfolders")
    List(s"sub${sep}c.cs", s"sub${sep}d.cs", "a.cs", "b.cs").foreach { testFile =>
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
    def apply(projectDir: File)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
        val cpg          = newEmptyCpg()
        val config       = Config().withInputPath(projectDir.toString).withOutputPath(tmpDir.toString)
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = CSharpSrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config, tmpDir)
        new AstCreationPass(cpg, astCreators, new Report()).createAndApply()
        f(cpg)
      }
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture(projectWithSubfolders) { cpg =>
      cpg.file.name.l should contain allElementsOf List("a.cs", "b.cs", s"sub${sep}c.cs", s"sub${sep}d.cs")
    }

  }

}
