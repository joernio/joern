package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class CallTests extends CSharpCode2CpgFixture {

  "builtin calls" should {

    val cpg = code(basicBoilerplate())

    "create a call node with arguments" in {
      val writeLine = cpg.call.nameExact("WriteLine").headOption match
        case Some(callNode) => callNode
        case None           => fail("Node not found!")

      writeLine.name shouldBe "WriteLine"
      writeLine.methodFullName shouldBe "System.Console.WriteLine:void(System.Object)"
      writeLine.typeFullName shouldBe "void"
      writeLine.code shouldBe "Console.WriteLine(\"Hello, world!\")"

      inside(writeLine.argument.l) {
        case (base: Identifier) :: (strArg: Literal) :: Nil =>
          base.typeFullName shouldBe "System.Console"
          base.name shouldBe "Console"
          base.code shouldBe "Console"
          base.argumentIndex shouldBe 0

          strArg.typeFullName shouldBe "System.String"
          strArg.code shouldBe "\"Hello, world!\""
          strArg.argumentIndex shouldBe 1
        case _ => fail("Arguments malformed or not found!")
      }
    }

    "be resolve a method full name without the definition clearly defined" in {}
  }

  "method invocations with await expression" should {
    val cpg = code("""
        |namespace Foo;
        |
        |public class Bar {
        | public int mBar(int pBar) {
        |   var getP = await new Baz().mBaz("hello");
        | }
        |}
        |
        |public class Baz {
        | public string mBaz(string pBaz) {
        |   return pBaz;
        | }
        |}
        |""".stripMargin)

    "create a call node for mBaz" in {
      inside(cpg.call.nameExact("mBaz").l) {
        case mBazCall :: Nil =>
          mBazCall.code shouldBe "new Baz().mBaz(\"hello\")"
          mBazCall.methodFullName shouldBe "Foo.Baz.mBaz:string(System.String)"
          mBazCall.typeFullName shouldBe "string"

        case _ => fail("No call node for `mBaz` found")
      }
    }
  }

  "method invocations with this expression" should {
    val cpg = code("""
        |namespace Foo;
        |
        |public class Bar {
        | public int mBar(int pBar) {
        |   var getBaz = this.mBaz("hello");
        | }
        | public string mBaz(string pBaz) {
        |   return pBaz;
        | }
        |}
        |
        |
        |""".stripMargin)

    "create a call node for mBaz" in {
      inside(cpg.call.nameExact("mBaz").l) {
        case mBazCall :: Nil =>
          mBazCall.code shouldBe "this.mBaz(\"hello\")"
          mBazCall.methodFullName shouldBe "Foo.Bar.mBaz:string(System.String)"
          mBazCall.typeFullName shouldBe "string"
        case _ => fail("No call node for `mBaz` found")
      }
    }

  }

  "hierarchical namespace calls" should {
    val cpg = code("""
        |namespace HelloWorld {
        |public class Foo {
        |}
        |
        |public class Bar: Foo {}
        |
        |public class Baz {}
        |}
        |""".stripMargin).moreCode("""
        |namespace HelloWorld.Foo {
        | public class A {
        |   static void main() {
        |     Bar c = new Bar();
        | }
        | }
        |}
        |""".stripMargin)

    "resolve type for Bar in a hierarchical namespace" in {
      inside(cpg.identifier.nameExact("c").l) {
        case c :: Nil =>
          c.typeFullName shouldBe "HelloWorld.Bar"
        case _ => fail("No identifier named `c` found")
      }
    }

  }
}
