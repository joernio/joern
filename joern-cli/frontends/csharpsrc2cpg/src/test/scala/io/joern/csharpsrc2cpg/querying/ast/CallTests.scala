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
      writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
      writeLine.typeFullName shouldBe "System.Void"
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
          mBazCall.methodFullName shouldBe "Foo.Baz.mBaz:System.String(System.String)"
          mBazCall.typeFullName shouldBe "System.String"

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
          mBazCall.methodFullName shouldBe "Foo.Bar.mBaz:System.String(System.String)"
          mBazCall.typeFullName shouldBe "System.String"
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

  "resolve a call with no receiver on a type sharing a base method inherited from a type in a common namespace" in {
    val cpg = code("""
        |namespace Foo.Bar.Bar {
        |  public class Baz: SomeClass {
        |     public async int SomeMethod() {
        |       var a = await SomeOtherMethod();
        |     }
        |  }
        |}
        |""".stripMargin).moreCode("""
        |namespace Foo.Bar.Bar {
        | public class SomeClass {
        |   protected int SomeOtherMethod() {
        |     return 1;
        |   }
        | }
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("SomeOtherMethod").l) {
      case callNode :: Nil =>
        callNode.code shouldBe "SomeOtherMethod()"
        callNode.typeFullName shouldBe "System.Int32"
        callNode.methodFullName shouldBe "Foo.Bar.Bar.SomeClass.SomeOtherMethod:System.Int32()"
      case _ => fail("No call for `SomeOtherMethod` found")
    }
  }

  "builtin types" should {
    val cpg = code("""
        |namespace Baz
        |{
        |  class Foo
        |  {
        |    static void Bar()
        |    {
        |      "".ToLower();
        |    }
        |  }
        |}
        |""".stripMargin)
    "resolve the ToLower call even without `using System`" ignore {
      inside(cpg.call.name("ToLower").methodFullName.l) {
        case x :: Nil => x shouldBe "System.String.ToLower:System.String()"
        case _        => fail("Unexpected call node structure")
      }
    }
  }

  "fully qualified names" should {
    val cpg = code("""
        |namespace Baz
        |{
        |  class Foo
        |  {
        |    static void Bar()
        |    {
        |      System.String x;
        |      x.ToLower();
        |    }
        |  }
        |}
        |""".stripMargin)
    "resolve the ToLower call even without `using System`" ignore {
      inside(cpg.call.name("ToLower").methodFullName.l) {
        case x :: Nil => x shouldBe "System.String.ToLower:System.String()"
        case _        => fail("Unexpected call node structure")
      }
    }
  }

}
