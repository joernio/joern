package io.shiftleft.fuzzyc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StubRemovalPassTests extends AnyWordSpec with Matchers {

  "StubRemovalPass" should {
    "remove stub if non-stub with same signature exists" in StubRemovalPassFixture("""
        |int foo(int x);
        |int foo(int x) {
        | return 0;
        |}
        |""".stripMargin) { cpg =>
      cpg.method.name.l shouldBe List("foo")
      cpg.method.isStub.l shouldBe List()
      cpg.parameter.name.l shouldBe List("x")
      cpg.methodReturn.l.size shouldBe 1
    }

    "remove stub even if even parameter names differ" in StubRemovalPassFixture("""
        |int foo(int another_name);
        |int foo(int x) {
        | return 0;
        |}
        |""".stripMargin) { cpg =>
      cpg.method.name.l shouldBe List("foo")
      cpg.method.isStub.l shouldBe List()
      cpg.parameter.name.l shouldBe List("x")
      cpg.methodReturn.l.size shouldBe 1
    }

    "keep multiple implementations" in StubRemovalPassFixture("""
        |int foo(int x) { return x; }
        |int foo(int x) {
        | return 0;
        |}
        |""".stripMargin) { cpg =>
      cpg.method.name.l shouldBe List("foo", "foo")
    }

  }

}

object StubRemovalPassFixture {
  def apply(file1Code: String)(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("fuzzyctest") { dir =>
      val file1 = (dir / "file1.c")
      file1.write(file1Code)
      val cpg = Cpg.emptyCpg
      val keyPool = new IntervalKeyPool(1001, 2000)
      val filenames = List(file1.path.toAbsolutePath.toString)
      new AstCreationPass(filenames, cpg, keyPool).createAndApply()
      new CfgCreationPass(cpg).createAndApply()
      new StubRemovalPass(cpg).createAndApply()
      f(cpg)
    }
  }
}
