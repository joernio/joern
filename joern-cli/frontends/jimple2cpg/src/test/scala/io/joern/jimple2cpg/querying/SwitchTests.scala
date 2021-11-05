package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.JumpTarget
import io.shiftleft.semanticcpg.language._

class SwitchTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """
      |class Foo {
      |
      |   public static String main(String[] args) {
      |       String animal = "DOG";
      |       String result;
      |       switch (animal) {
      |           case "DOG":
      |               result = "domestic animal";
      |               break;
      |           case "CAT":
      |               result = "domestic animal";
      |               break;
      |           case "TIGER":
      |               result = "wild animal";
      |               break;
      |           default:
      |               result = "unknown animal";
      |               break;
      |       }
      |       return result;
      |   }
      |
      |}
      |""".stripMargin

  "should identify switch roots" in {
    cpg.method.name("main").switchBlock.code.toSet shouldBe Set(
      "tableswitch(l4)",
      "lookupswitch($stack5)"
    )
  }

  "should have 3 ordinary cases and a default under the table switch" in {
    cpg.jumpTarget.filter(_.astParent.code == "tableswitch(l4)").name.toSet shouldBe Set(
      "default",
      "case 0",
      "case 1",
      "case 2"
    )
  }

  "should have 3 cases holding string hashcodes and a default under the lookup switch" in {
    cpg.jumpTarget.filter(_.astParent.code == "lookupswitch($stack5)").name.toSet shouldBe Set(
      "case 79820959",
      "default",
      "case 67868",
      "case 66486"
    )
  }

  "should flow jump targets from switches" in {
    cpg.switchBlock
      .filter(_.code == "tableswitch(l4)")
      .astChildren
      .collect { case a: JumpTarget => a }
      .code
      .toSet shouldBe Set(
      "case 1:",
      "case 2:",
      "case 0:",
      "default:"
    )
    cpg.switchBlock
      .filter(_.code == "lookupswitch($stack5)")
      .astChildren
      .collect { case a: JumpTarget => a }
      .code
      .toSet shouldBe Set(
      "case 66486:",
      "case 79820959:",
      "case 67868:",
      "default:"
    )
  }

  "should flow jump conditionals from switches" in {
    cpg.switchBlock
      .filter(_.code == "tableswitch(l4)")
      .condition
      .code
      .toSet shouldBe Set(
      "l4"
    )
    cpg.switchBlock
      .filter(_.code == "lookupswitch($stack5)")
      .condition
      .code
      .toSet shouldBe Set(
      "$stack5"
    )
  }

}
