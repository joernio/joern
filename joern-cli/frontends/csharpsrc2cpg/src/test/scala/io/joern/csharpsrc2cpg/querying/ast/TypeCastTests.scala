package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class TypeCastTests extends CSharpCode2CpgFixture {
  "a type cast expression for literals on the RHS of a equals value clause" should {
    val cpg = code(basicBoilerplate("""
        |var a = (int)1.0;
        |""".stripMargin))

    "set the original type for the RHS" in {
      inside(cpg.literal.codeExact("1.0").l) {
        case lit :: Nil =>
          lit.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Double)
        case _ => fail("No literal with code `1.0` found")
      }
    }

    "set the casted type on the LHS" in {
      inside(cpg.identifier.nameExact("a").l) {
        case ident :: Nil =>
          ident.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
        case _ => fail("No identifier named `a` found")
      }
    }

    "create a cast operator node" in {
      inside(cpg.call.nameExact(Operators.cast).l) {
        case castCall :: Nil =>
          castCall.code shouldBe "(int)1.0"
          castCall.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
          castCall.lineNumber shouldBe Some(9)
          castCall.columnNumber shouldBe Some(8)
        case _ => fail("Call node for cast operator not found")
      }
    }
  }

  "a type cast expression for custom types" should {
    val cpg = code("""
        |namespace Foo {
        | public class Bar {}
        | public class Baz: Bar {}
        | public class Bazz {
        |   public static void Main() {
        |     var b = (Bar)new Baz();
        |   }
        | }
        |}
        |""".stripMargin)

    "set the original type for the RHS" in {
      inside(cpg.call.nameExact("<init>").l) {
        case constrCall :: Nil =>
          constrCall.typeFullName shouldBe "Foo.Baz"
        case _ => fail("No constructor call for `Baz` found")
      }
    }

    "set the casted type on the LHS" in {
      inside(cpg.identifier.nameExact("b").l) {
        case ident :: Nil =>
          ident.typeFullName shouldBe "Foo.Bar"
        case _ => fail("No identifier named `b` found")
      }
    }

    "create a cast operator node" in {
      inside(cpg.call.nameExact(Operators.cast).l) {
        case castCall :: Nil =>
          castCall.code shouldBe "(Bar)new Baz()"
          castCall.typeFullName shouldBe "Foo.Bar"
          castCall.lineNumber shouldBe Some(6)
          castCall.columnNumber shouldBe Some(13)
        case _ => fail("Call node for cast operator not found")
      }
    }
  }

}
