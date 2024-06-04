package io.joern.csharpsrc2cpg.io

import better.files.File
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.csharpsrc2cpg.passes.AstCreationPass
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

class ProjectParseTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private val sep = java.io.File.separator

  private val projectWithSubfolders: File = {
    val dir = File.newTemporaryDirectory("csharpsrc2cpgTestsSubfolders")
    List(s"sub${sep}c.cs", s"sub${sep}d.cs", "a.cs", "b.cs").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      file.write(basicBoilerplate())
    }
    dir
  }

  private val projectWithBrokenFile: File = {
    val dir      = File.newTemporaryDirectory("csharpsrc2cpgTestsBroken")
    val goodFile = dir / "good.cs"
    goodFile.createIfNotExists(createParents = true)
    goodFile.write(basicBoilerplate("Console.WriteLine(\"Good\");"))
    val brokenFile = dir / "broken.cs"
    brokenFile.createIfNotExists(createParents = true)
    brokenFile.write(basicBoilerplate("Console.WriteLi\"Broken\""))
    dir
  }

  private val projectWithUtf8: File = {
    val dir  = File.newTemporaryDirectory("csharpsrc2cpgTestsUtf8")
    val file = dir / "utf8.cs"
    file.createIfNotExists(createParents = true)
    file.write(basicBoilerplate("// 😼"))
    dir
  }

  override def afterAll(): Unit = {
    projectWithSubfolders.delete(swallowIOExceptions = true)
    projectWithBrokenFile.delete(swallowIOExceptions = true)
    projectWithUtf8.delete(swallowIOExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: File)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
        val cpg          = newEmptyCpg()
        val config       = Config().withInputPath(projectDir.toString).withOutputPath(tmpDir.toString)
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = CSharpSrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config)
        new AstCreationPass(cpg, astCreators, new Report()).createAndApply()
        f(cpg)
      }
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture(projectWithSubfolders) { cpg =>
      cpg.file.name.l should contain allElementsOf List("a.cs", "b.cs", s"sub${sep}c.cs", s"sub${sep}d.cs")
    }

    "recover from broken input file" in ProjectParseTestsFixture(projectWithBrokenFile) { cpg =>
      cpg.file.name.l should (contain("good.cs") and not contain "broken.cs")
    }

    "handle utf8 correctly" in ProjectParseTestsFixture(projectWithUtf8) { cpg =>
      cpg.file.name.l should contain("utf8.cs")
    }

  }

}
