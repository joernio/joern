package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class MemberAccessTests extends CSharpCode2CpgFixture {
  "conditional member access expressions" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux {get;}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var a = baz?.Qux;
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1)) {
        case a :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
              rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
            case _ => fail("Expected 2 arguments under the assignment call.")
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }

  "conditional method access expressions" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux() {}
        |   public string Fred(int a) {}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var a = baz?.Qux();
        |     var b = baz?.Fred(1);
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types and attributes both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1)) {
        case a :: b :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
              rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
            case _ => fail("Expected 2 arguments under the assignment call")
          }

          inside(b.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)
              rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)
            case _ => fail("Expected 2 arguments under the assignment call")
          }

          inside(cpg.call.nameExact("Fred").l) {
            case fred :: Nil =>
              fred.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)
              fred.methodFullName shouldBe "Foo.Baz.Fred:System.String(System.Int32)"
            case _ => fail("Expected a call named `Fred`")

          }
        case _ => fail("Expected 2 assignment call.")
      }
    }
  }
}
