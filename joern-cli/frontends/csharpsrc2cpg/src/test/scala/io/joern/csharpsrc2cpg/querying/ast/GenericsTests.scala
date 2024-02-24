package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class GenericsTests extends CSharpCode2CpgFixture {
  "generic type names" should {
    val cpg = code("""
        |namespace Foo {
        | public interface IBar {}
        | public class Bar<T> {}
        | public class Baz {
        |   static void Test() {
        |     Bar<IBar> a = new Bar<IBar>();
        |     var b = new Bar<IBar>().someMethod();
        |   }
        | }
        |}
        |
        |""".stripMargin)

    "propagate types from RHS to LHS" in {
      inside(cpg.identifier.nameExact("a").headOption) {
        case Some(a) =>
          a.name shouldBe "a"
          a.typeFullName shouldBe "Foo.Bar"
        case None => fail("Unable to find `a` identifier node with type declaration")
      }
    }
  }
}
