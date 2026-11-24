package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.{Config, Rust2Cpg}
import io.joern.x2cpg.astgen.AstGenRunner.AstGenFailedException
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files
import scala.util.{Failure, Success}

class AstGenFailureTests extends AnyWordSpec with Matchers {

  "Rust2Cpg" should {

    "fail instead of producing an empty CPG if the AST generator crashes" in {
      FileUtil.usingTemporaryDirectory("rust2cpgFailureIn") { inputDir =>
        // A manifest that cannot be parsed: rust_ast_gen exits with an error without writing any output, independent of
        // the toolchain installed on the machine.
        Files.writeString(inputDir / "Cargo.toml", "this is not a valid manifest")
        (inputDir / "src").createWithParentsIfNotExists(asDirectory = true, createParents = true)
        Files.writeString(inputDir / "src" / "lib.rs", "pub fn f(a: i32) -> i32 { a + 1 }\n")

        FileUtil.usingTemporaryDirectory("rust2cpgFailureOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath((outputDir / "cpg.bin").toString)
          new Rust2Cpg().createCpg(config) match {
            case Failure(exception) =>
              exception shouldBe a[AstGenFailedException]
            case Success(cpg) =>
              cpg.close()
              fail("createCpg succeeded although the AST generator crashed")
          }
        }
      }
    }
  }

}
