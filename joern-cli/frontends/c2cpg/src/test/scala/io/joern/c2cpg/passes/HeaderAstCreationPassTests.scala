package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.fixtures.TestProjectFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HeaderAstCreationPassTests extends AnyWordSpec with Matchers with Inside {

  private val fixture: TestProjectFixture = TestProjectFixture("includes")

  "HeaderAstCreationPass" should {

    "create all source and header files correctly" in {
      val fileNames = fixture.cpg.file.nameNot(".*<includes>|<unknown>").name.sorted.map(File(_).name)
      fileNames shouldBe Seq("main.c", "main.h", "other.h")
    }

    "de-duplicate content correctly" in {
      inside(fixture.cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).sortBy(_.fullName)) {
        case Seq(bar, foo, m1, m2) =>
          // note that we don't see bar twice even so it is contained
          // in main.h and included in main.c and we do scan both
          bar.fullName shouldBe "bar"
          bar.filename should endWith("main.h")
          foo.fullName shouldBe "foo"
          foo.filename should endWith("other.h")
          // main is include twice. First time for the header file,
          // second time for the actual implementation in the source file
          // We do not de-duplicate this as line/column numbers differ
          m1.fullName shouldBe "main"
          m1.filename should endWith("main.h")
          m2.fullName shouldBe "main"
          m2.filename should endWith("main.c")
      }
    }

  }

}
