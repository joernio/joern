package io.joern.fuzzyc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.frontend.TypeNodePass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class TypeNodePassTests extends AnyWordSpec with Matchers {
  "TypeNodePass" should {
    "create TYPE nodes for used types" in TypeNodePassFixture("int main() { int x; }") { cpg =>
      cpg.typ.name.toSet shouldBe Set("int", "void", "ANY")
    }
  }
}

object TypeNodePassFixture {
  def apply(file1Code: String)(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("fuzzyctest") { dir =>
      val file1 = dir / "file1.c"
      file1.write(file1Code)

      val cpg = Cpg.emptyCpg
      val keyPool = new IntervalKeyPool(1001, 2000)
      val filenames = List(file1.path.toAbsolutePath.toString)
      val astCreator = new AstCreationPass(filenames, cpg, keyPool)
      astCreator.createAndApply()
      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg).createAndApply()

      f(cpg)
    }
  }
}
