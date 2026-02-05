package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.passes.AstCreationPass
import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Files, Path}

class ProjectParseTests extends SwiftSrc2CpgSuite with BeforeAndAfterAll {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  private val projectWithUtf8: Path = {
    val dir  = Files.createTempDirectory("swiftsrc2cpgTestsUtf8")
    val file = dir / "utf8.swift"
    file.createWithParentsIfNotExists(createParents = true)
    val content = """
      |import Foundation
      |
      |// 🚀
      |// 😼
      |// ❌
      |// 1️⃣ Some comment ...
      |func main() {
      |  print("6️⃣ Something: \(foo?.bar ?? 0)")  // 💥 May crash
      |  print("✅ Done!")
      |}
      |""".stripMargin
    Files.writeString(file, content)
    dir
  }

  override def afterAll(): Unit = {
    FileUtil.delete(projectWithUtf8, swallowIoExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: Path)(f: Cpg => Unit): Unit = {
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgTests") { tmpDir =>
        val cpg = newEmptyCpg()
        val config = Config()
          .withInputPath(projectDir.toString)
          .withOutputPath(tmpDir.toString)
          .withDisableFileContent(false)
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        f(cpg)
      }
    }
  }

  "Parsing a project" should {

    "handle utf8 correctly" in ProjectParseTestsFixture(projectWithUtf8) { cpg =>
      val List(op) = cpg.call.nameExact(Operators.elvis).l
      op.offset shouldBe Some(115)
      op.offsetEnd shouldBe Some(128)
      cpg.method.nameExact("main").content.head.linesIterator.map(_.trim).toSeq shouldBe Seq(
        "func main() {",
        "print(\"6?????? Something: \\(foo?.bar ?? 0)\")  // ???? May crash",
        "print(\"??? Done!\")",
        "}"
      )
    }

  }

}
