package io.joern.x2cpg.testfixtures

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.collection.mutable

class Code2CpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cleanupRegister = mutable.ArrayBuffer.empty[(Path, TestCpg)]

  def code(code: String): TestCpg = {
    new TestCpg(frontend, this.registerTmpDir).moreCode(code)
  }

  def code(code: String, fileName: String): TestCpg = {
    new TestCpg(frontend, this.registerTmpDir).moreCode(code, fileName)
  }

  private def registerTmpDir(tmpDir: Path, cpg: TestCpg): Unit = {
    cleanupRegister.append((tmpDir, cpg))
  }

  override def afterAll(): Unit = {
    cleanupRegister.foreach { case (tmpDir, cpg) =>
      cpg.close()

      Files
        .walk(tmpDir)
        .sorted(Comparator.reverseOrder[Path]())
        .forEach(Files.delete(_))
    }
  }
}
