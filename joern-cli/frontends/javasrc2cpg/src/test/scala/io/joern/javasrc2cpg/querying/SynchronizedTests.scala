package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Identifier,
  Method,
  MethodParameterIn,
  MethodReturn,
  Modifier,
  Return
}
import io.shiftleft.semanticcpg.language._

class SynchronizedTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |public class Foo {
      |  public static synchronized String foo(String s) {
      |    return s;
      |  }
      |
      |  public static String bar(String s) {
      |    synchronized (this) {
      |      s += "A";
      |    }
      |    return s;
      |  }
      |
      |}
      |""".stripMargin

  "it should process a synchronized method the same as a non-synchronized method" in {
    val List(method: Method) = cpg.method.name("foo").l

    method.astChildren.size shouldBe 4
    val List(param: MethodParameterIn, _, body: Block, _: MethodReturn) = method.astChildren.l
    param.code shouldBe "String s"
    body.astChildren.head shouldBe a[Return]
  }

  "it should create a synchronized block" in {
    val List(method: Method)   = cpg.method.name("bar").l
    val List(syncBlock: Block) = method.ast.isBlock.where(_.astChildren.isModifier.modifierType("SYNCHRONIZED")).l

    syncBlock.astChildren.size shouldBe 3
    val List(mod: Modifier, cond: Identifier, body: Block) = syncBlock.astChildren.l

    mod.modifierType shouldBe "SYNCHRONIZED"
    cond.code shouldBe "this"
    body.astChildren.head.code shouldBe "s += \"A\""
  }
}
