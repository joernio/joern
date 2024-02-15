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
        |   Bar<IBar> b = new Bar<IBar>();
        |   var b = new Bar<IBar>().someMethod();
        | }
        |}
        |""".stripMargin)

    "propagate types from RHS to LHS" in {
      println(cpg.call.typeFullName.l)
    }
  }

}
