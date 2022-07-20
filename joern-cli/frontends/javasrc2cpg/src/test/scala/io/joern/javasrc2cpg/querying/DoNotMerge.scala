package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class DoNotMerge extends JavaSrcCode2CpgFixture {

  "it should not crash" in {
    val cpg = code("""
        |import java.util.Function;
        |import a.Unknown;
        |import a.AlsoUnknown;
        |
        |@Override
        |public class Foo implements Function<Unknown, AlsoUnknown> {
        |  public AlsoUnknown apply(Unknown unknown) {
        |    return new AlsoUnknown(unknown);
        |  }
        |}
        |""".stripMargin)
    cpg.method.name("apply").parameter.map(_.typeFullName).foreach(println)
    cpg.typeDecl.name("Foo").isEmpty shouldBe true
  }

}
