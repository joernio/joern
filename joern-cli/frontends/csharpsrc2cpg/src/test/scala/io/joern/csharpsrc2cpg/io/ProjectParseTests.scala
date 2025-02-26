package io.joern.csharpsrc2cpg.io

import better.files.File
import io.joern.csharpsrc2cpg.CSharpSrc2Cpg
import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.passes.AstCreationPass
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.{Report, FileUtil}
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

import java.io.File as JFile
import java.nio.file.Files
import java.nio.charset.Charset

class ProjectParseTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private val sep = java.io.File.separator

  private val projectWithSubfolders: JFile = {
    val dir = FileUtil.newTemporaryDirectory("csharpsrc2cpgTestsSubfolders")
    List(s"sub${sep}c.cs", s"sub${sep}d.cs", "a.cs", "b.cs").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      Files.write(file.toPath, basicBoilerplate().getBytes(Charset.defaultCharset()))
    }
    dir
  }

  private val projectWithBrokenFile: JFile = {
    val dir      = FileUtil.newTemporaryDirectory("csharpsrc2cpgTestsBroken")
    val goodFile = dir / "good.cs"
    goodFile.createIfNotExists(createParents = true)
    Files.write(goodFile.toPath, basicBoilerplate("Console.WriteLine(\"Good\");").getBytes(Charset.defaultCharset()))
    val brokenFile = dir / "broken.cs"
    brokenFile.createIfNotExists(createParents = true)
    Files.write(brokenFile.toPath, basicBoilerplate("Console.WriteLi\"Broken\"").getBytes(Charset.defaultCharset()))
    dir
  }

  private val projectWithUtf8: JFile = {
    val dir  = FileUtil.newTemporaryDirectory("csharpsrc2cpgTestsUtf8")
    val file = dir / "utf8.cs"
    file.createIfNotExists(createParents = true)
    Files.write(file.toPath, basicBoilerplate("// 😼").getBytes(Charset.defaultCharset()))
    dir
  }

  override def afterAll(): Unit = {
    FileUtil.deleteFile(projectWithSubfolders, swallowIoExceptions = true)
    FileUtil.deleteFile(projectWithBrokenFile, swallowIoExceptions = true)
    FileUtil.deleteFile(projectWithUtf8, swallowIoExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: JFile)(f: Cpg => Unit): Unit = {
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
