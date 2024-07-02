package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Identifier,
  Method,
  MethodParameterIn,
  MethodReturn,
  Modifier,
  Return
}
import io.shiftleft.semanticcpg.language.*

class SynchronizedTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
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
      |""".stripMargin)

  "it should process a synchronized method the same as a non-synchronized method" in {
    val List(method: Method) = cpg.method.name("foo").l

    method.astChildren.size shouldBe 6
    val List(
      param: MethodParameterIn,
      _,
      body: Block,
      publicModifier: Modifier,
      staticModifier: Modifier,
      _: MethodReturn
    ) = method.astChildren.l: @unchecked
    param.code shouldBe "String s"
    body.astChildren.head shouldBe a[Return]
    publicModifier.modifierType shouldBe ModifierTypes.PUBLIC
    staticModifier.modifierType shouldBe ModifierTypes.STATIC
  }

  "it should create a synchronized block" in {
    val List(method: Method)   = cpg.method.name("bar").l
    val List(syncBlock: Block) = method.ast.isBlock.where(_.astChildren.isModifier.modifierType("SYNCHRONIZED")).l

    syncBlock.astChildren.size shouldBe 3
    val List(mod: Modifier, cond: Identifier, body: Block) = syncBlock.astChildren.l: @unchecked

    mod.modifierType shouldBe "SYNCHRONIZED"
    cond.code shouldBe "this"
    body.astChildren.head.code shouldBe "s += \"A\""
  }
}
