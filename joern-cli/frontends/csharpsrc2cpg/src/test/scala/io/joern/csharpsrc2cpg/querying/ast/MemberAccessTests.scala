package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class MemberAccessTests extends CSharpCode2CpgFixture {

  "conditional property access expressions" should {
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

  "conditional member access expressions" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux;
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

  "chained field access expression referencing members of a sibling class" should {
    val cpg = code("""
        |namespace Foo;
        |class Bar
        |{
        | Bar Field1;
        | Bar Field2;
        |}
        |class Baz
        |{
        | static void DoStuff()
        | {
        |   var x = new Bar();
        |   var y = x.Field1.Field2;
        | }
        |}
        |
        |""".stripMargin)

    "have correct typeDecls" in {
      cpg.typeDecl.nameExact("Bar").size shouldBe 1
    }

    "have correct properties for the innermost member" in {
      cpg.typeDecl.nameExact("Bar").member.nameExact("Field1").typeFullName.l shouldBe List("Foo.Bar")
    }

    "have correct properties for the outermost member" in {
      cpg.typeDecl.nameExact("Bar").member.nameExact("Field2").typeFullName.l shouldBe List("Foo.Bar")
    }

    "have correct properties for the outermost field access" in {
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Field2")).l) {
        case field2 :: Nil =>
          field2.typeFullName shouldBe "Foo.Bar"
          field2.referencedMember.l shouldBe cpg.typeDecl.nameExact("Bar").member.nameExact("Field2").l
        case _ => fail("Expected single field access to `Field2`")
      }
    }

    "have correct properties for the innermost field access" in {
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Field1")).l) {
        case field1 :: Nil =>
          field1.typeFullName shouldBe "Foo.Bar"
          field1.referencedMember.l shouldBe cpg.typeDecl.nameExact("Bar").member.nameExact("Field1").l
        case _ => fail("Expected single field access to `Field1`")
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

  "null-forgiving member access expressions" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux {get;}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var a = baz!.Qux;
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1)) {
        case a :: Nil =>
          inside(a.argument.l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
            rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }

  "null-forgiving method access expressions" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux() {}
        |   public string Fred(int a) {}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var a = baz!.Qux();
        |     var b = baz!.Fred(1);
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

  "conditional method access expressions for chained calls" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public int Qux() {}
        |   public Baz Fred(int a) {}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var b = baz?.Fred(1)?.Fred(2)?.Qux();
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types and attributes both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1).l) {
        case a :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
              rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
            case _ => fail("Expected 2 arguments under the assignment call")
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }

  "chained calls for fields and methods together" should {
    "combination of method access expression for chained fields" should {
      val cpg = code("""
          |namespace Foo {
          | public class Baz {
          |   public Baz Qux {get;}
          |   public int Fred() {}
          | }
          | public class Bar {
          |   public static void Main() {
          |     var baz = new Baz();
          |     var b = baz.Qux.Qux.Qux.Fred();
          |   }
          | }
          |}
          |""".stripMargin)

      "have correct types and attributes both on the LHS and RHS" in {
        inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1).l) {
          case a :: Nil =>
            inside(a.argument.l) {
              case (lhs: Identifier) :: (rhs: Call) :: Nil =>
                lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
                rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
              case _ => fail("Expected 2 arguments under the assignment call")
            }
          case _ => fail("Expected 1 assignment call.")
        }
      }
    }
  }

  // TODO: ConditionalAccessExpressions need some work to deal with nested chains.
  // This particular test-case relies on the usage of getters, that are currently being
  // reworked to be METHODs instead of MEMBERs.
  // Revisit this test-case once getters are finished.
  "conditional property access expression for chained fields" ignore {
    val cpg = code("""
      |namespace Foo {
      | public class Baz {
      |   public Baz Qux {get;}
      | }
      | public class Bar {
      |   public static void Main() {
      |     var baz = new Baz();
      |     var b = baz?.Qux?.Qux;
      |   }
      | }
      |}
      |""".stripMargin)

    "have correct types and attributes both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1).l) {
        case a :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe "Foo.Baz"
              rhs.typeFullName shouldBe "Foo.Baz"
            case _ => fail("Expected 2 arguments under the assignment call")
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }

  "conditional method access expression for chained fields" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public Baz Qux;
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var b = baz?.Qux?.Qux;
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types and attributes both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1).l) {
        case a :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe "Foo.Baz"
              rhs.typeFullName shouldBe "Foo.Baz"
            case _ => fail("Expected 2 arguments under the assignment call")
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }

  "combination of method access expression for chained fields" should {
    val cpg = code("""
        |namespace Foo {
        | public class Baz {
        |   public Baz Qux {get;}
        |   public int Fred() {}
        | }
        | public class Bar {
        |   public static void Main() {
        |     var baz = new Baz();
        |     var b = baz.Qux?.Qux!.Qux.Fred();
        |   }
        | }
        |}
        |""".stripMargin)

    "have correct types and attributes both on the LHS and RHS" in {
      inside(cpg.assignment.l.sortBy(_.lineNumber).drop(1).l) {
        case a :: Nil =>
          inside(a.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
              rhs.typeFullName shouldBe BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
            case _ => fail("Expected 2 arguments under the assignment call")
          }
        case _ => fail("Expected 1 assignment call.")
      }
    }
  }
}
