package io.joern.csharpsrc2cpg.io

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

import java.nio.file.{Files, Path}

class ProjectParseTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private val sep = java.io.File.separator

  private val projectWithSubfolders: Path = {
    val dir = Files.createTempDirectory("csharpsrc2cpgTestsSubfolders")
    List(s"sub${sep}c.cs", s"sub${sep}d.cs", "a.cs", "b.cs").foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, basicBoilerplate())
    }
    dir
  }

  private val projectWithBrokenFile: Path = {
    val dir      = Files.createTempDirectory("csharpsrc2cpgTestsBroken")
    val goodFile = dir / "good.cs"
    goodFile.createWithParentsIfNotExists(createParents = true)
    Files.writeString(goodFile, basicBoilerplate("Console.WriteLine(\"Good\");"))
    val brokenFile = dir / "broken.cs"
    brokenFile.createWithParentsIfNotExists(createParents = true)
    Files.writeString(brokenFile, basicBoilerplate("Console.WriteLi\"Broken\""))
    dir
  }

  private val projectWithUtf8: Path = {
    val dir  = Files.createTempDirectory("csharpsrc2cpgTestsUtf8")
    val file = dir / "utf8.cs"
    file.createWithParentsIfNotExists(createParents = true)
    Files.writeString(file, basicBoilerplate("// 😼"))
    dir
  }

  override def afterAll(): Unit = {
    FileUtil.delete(projectWithSubfolders, swallowIoExceptions = true)
    FileUtil.delete(projectWithBrokenFile, swallowIoExceptions = true)
    FileUtil.delete(projectWithUtf8, swallowIoExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: Path)(f: Cpg => Unit): Unit = {
      FileUtil.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
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
