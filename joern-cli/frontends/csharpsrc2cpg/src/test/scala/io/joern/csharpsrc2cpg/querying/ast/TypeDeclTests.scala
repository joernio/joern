package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.x2cpg.Defines

class TypeDeclTests extends CSharpCode2CpgFixture {

  "a basic class declaration" should {
    val cpg = code("public class Container {  }", "Container.cs")

    "generate a type declaration with the correct properties" in {
      val x = cpg.typeDecl.nameExact("Container").head
      x.code shouldBe "public class Container"
      x.fullName shouldBe "Container"
      x.filename shouldBe "Container.cs"
      x.aliasTypeFullName shouldBe None
      x.inheritsFromTypeFullName shouldBe Seq.empty
    }

    "generate a type declaration with the correct modifiers" in {
      val x = cpg.typeDecl.nameExact("Container").head
      x.modifier.modifierType.head shouldBe ModifierTypes.PUBLIC
    }
  }

  "a basic class declaration within a namespace" should {
    val cpg = code(
      """namespace SampleNamespace
        |{
        |    private class SampleClass { }
        |}
        |""".stripMargin,
      "SampleClass.cs"
    )

    "generate a type declaration with the correct properties" in {
      val x = cpg.typeDecl.nameExact("SampleClass").head
      x.code shouldBe "private class SampleClass"
      x.fullName shouldBe "SampleNamespace.SampleClass"
      x.filename shouldBe "SampleClass.cs"
      x.aliasTypeFullName shouldBe None
      x.inheritsFromTypeFullName shouldBe Seq.empty
    }

    "generate a type declaration with the correct modifiers" in {
      val x = cpg.typeDecl.nameExact("SampleClass").head
      x.modifier.modifierType.head shouldBe ModifierTypes.PRIVATE
    }
  }

  "a basic struct declaration" should {
    val cpg = code("""
        |public struct Coords
        |{
        |    public double y;
        |}
        |""".stripMargin)

    "generate a type declaration with correct properties" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.fullName shouldBe "Coords"
        case None         => fail("Unable to find `Coords` type decl node")

      }
    }

    "generate a type declaration with correct modifiers" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.modifier.modifierType.head shouldBe ModifierTypes.PUBLIC
        case None         => fail("Unable to find modifier for `Coords` type decl node")
      }
    }

    "generate a type declaration with correct member" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.member.name.head shouldBe "y"
        case None         => fail("Unable to find member for `Coords` type decl node")
      }
    }
  }

  "basic records declaration" should {
    val cpg = code("""
        |private record Person(string Name, string Mood);
        |
        |public record Car
        |{
        |    public string Model;
        |    public string Year;
        |};
        |""".stripMargin)

    "generate a type declaration with properties for first declaration style" in {
      inside(cpg.typeDecl.nameExact("Person").headOption) {
        case Some(rec) =>
          rec.fullName shouldBe "Person"
          rec.member.name.head shouldBe "Name"
          rec.member.name.last shouldBe "Mood"
          rec.modifier.modifierType.head shouldBe ModifierTypes.PRIVATE
        case None => fail("Unable to find `Person` type decl node with correct properties")

      }
    }

    "generate a type declaration with properties for second declaration style" in {
      inside(cpg.typeDecl.nameExact("Car").headOption) {
        case Some(rec) =>
          rec.fullName shouldBe "Car"
          rec.member.name.head shouldBe "Model"
          rec.member.name.last shouldBe "Year"
          rec.modifier.modifierType.head shouldBe ModifierTypes.PUBLIC
        case None => fail("Unable to find `Car` type decl node with correct properties")

      }
    }
  }

  "basic enum types" should {

    val cpg = code(
      """
        |enum Season
        |{
        |    Spring,
        |    Summer,
        |    Autumn,
        |    Winter
        |}
        |""".stripMargin,
      "Season.cs"
    )

    "generate a type declaration enum members" in {
      inside(cpg.typeDecl.nameExact("Season").headOption) {
        case Some(season) =>
          season.fullName shouldBe "Season"
          inside(season.member.l) {
            case xs if xs.isEmpty => fail("No enum members found!")
            case spring :: summer :: autumn :: winter :: Nil =>
              spring.name shouldBe "Spring"
              spring.code shouldBe "Spring"
              spring.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)

              summer.name shouldBe "Summer"
              summer.code shouldBe "Summer"
              summer.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)

              autumn.name shouldBe "Autumn"
              autumn.code shouldBe "Autumn"
              autumn.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)

              winter.name shouldBe "Winter"
              winter.code shouldBe "Winter"
              winter.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.Int)
            case _ => fail("Unexpected number of enum members!")
          }
        case None => fail("Unable to find `Season` type decl node")
      }
    }
  }

  "enum types cast as an integer type" should {

    val cpg = code("""
        |enum ErrorCode : ushort
        |{
        |    None = 0,
        |    Unknown = 1,
        |    ConnectionLost = 100,
        |    OutlierReading = 200
        |}
        |
        |""".stripMargin)

    "generate a type declaration enum members" in {
      inside(cpg.typeDecl.nameExact("ErrorCode").headOption) {
        case Some(errCode) =>
          errCode.fullName shouldBe "ErrorCode"
          inside(errCode.member.l) {
            case Nil => fail("No enum members found!")
            case none :: unknown :: connectionLost :: outlierReading :: Nil =>
              none.name shouldBe "None"
              none.code shouldBe "None = 0"
              none.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.UShort)

              unknown.name shouldBe "Unknown"
              unknown.code shouldBe "Unknown = 1"
              unknown.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.UShort)

              connectionLost.name shouldBe "ConnectionLost"
              connectionLost.code shouldBe "ConnectionLost = 100"
              connectionLost.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.UShort)

              outlierReading.name shouldBe "OutlierReading"
              outlierReading.code shouldBe "OutlierReading = 200"
              outlierReading.typeFullName shouldBe DotNetTypeMap(BuiltinTypes.UShort)
            case _ => fail("Unexpected number of enum members!")
          }
        case None => fail("Unable to find `ErrorCode` type decl node")
      }
    }

    // TODO: Requires <clinit> issue to be done
    "initialize the members in a <clinit> class" ignore {
      inside(cpg.typeDecl.nameExact("ErrorCode").method.nameExact(Defines.StaticInitMethodName).l) {
        case m :: Nil =>
          m.fullName shouldBe s"ErrorCode.${Defines.StaticInitMethodName}"
          inside(m.assignment.l) {
            case none :: unknown :: connectionLost :: outlierReading :: Nil =>
              none.code shouldBe "None = 0"
              unknown.code shouldBe "Unknown = 1"
              connectionLost.code shouldBe "ConnectionLost = 100"
              outlierReading.code shouldBe "OutlierReading = 200"
            case _ => fail("Exactly 4 assignments expected")
          }
        case _ => fail("`ErrorCode` has no static initializer method!")
      }
    }

  }

  "an interface" should {

    val cpg = code("""
        |namespace Foo {
        |
        | interface ISampleInterface
        | {
        |     void SampleMethod();
        | }
        |
        | class ImplementationClass : ISampleInterface
        | {
        |     // Explicit interface member implementation:
        |     void ISampleInterface.SampleMethod()
        |     {
        |         // Method implementation.
        |      }
        |
        |      static void Main()
        |      {
        |          // Declare an interface instance.
        |          ISampleInterface obj = new ImplementationClass();
        |
        |         // Call the member.
        |          obj.SampleMethod();
        |      }
        | }
        |
        |}
        |""".stripMargin)

    "have a corresponding TYPE_DECL node" in {
      inside(cpg.typeDecl.name("ISampleInterface").headOption) {
        case Some(typeDecl) =>
          typeDecl.fullName shouldBe "Foo.ISampleInterface"
          typeDecl.code shouldBe "interface ISampleInterface"
        case None => fail("No interface type declaration node found!")
      }
    }

    "have a child method" in {
      inside(cpg.typeDecl.name("ISampleInterface").method.l) {
        case sampleMethod :: Nil =>
          sampleMethod.name shouldBe "SampleMethod"
          sampleMethod.code shouldBe "void SampleMethod();"
        case _ => fail("No child method found for interface!")
      }
    }

    "be inherited by the implementation class" ignore {
      inside(cpg.typeDecl.name("ISampleInterface", "ImplementationClass").l) {
        case interface :: implementation :: Nil => // TODO: For issue #3992
        case _                                  => fail("Unexpected number of type declarations")
      }
    }
  }

  "an anonymous type with primitive type members" should {
    val cpg = code(basicBoilerplate("""
        |var Foo = new { Bar = 10, Baz = "Hello, World" };
        |""".stripMargin))

    "create a TypeDecl node" in {
      inside(cpg.method("Main").astChildren.isTypeDecl.l) {
        case anonType :: Nil =>
          anonType.fullName shouldBe "HelloWorld.Program.Main.<anon>0"
          anonType.astParentType shouldBe "METHOD"
          anonType.astParentFullName shouldBe "HelloWorld.Program.Main:System.Void(System.String[])"
        case _ => fail("No TypeDecl node for anonymous object found")
      }
    }

    "propagate type to the LHS" in {
      inside(cpg.method("Main").astChildren.isBlock.astChildren.isLocal.nameExact("Foo").l) { case loc :: Nil =>
        loc.typeFullName shouldBe "HelloWorld.Program.Main.<anon>0"
      }
    }

    "have correct members" in {
      inside(cpg.method("Main").astChildren.isTypeDecl.l) {
        case anonType :: Nil =>
          inside(anonType.astChildren.isMember.l) {
            case bar :: baz :: Nil =>
              bar.code shouldBe "Bar = 10"
              baz.code shouldBe "Baz = \"Hello, World\""

              bar.typeFullName shouldBe "System.Int32"
              baz.typeFullName shouldBe "System.String"

              bar.astParent shouldBe anonType
              baz.astParent shouldBe anonType
            case _ => fail("There should be exactly 2 members inside the anonymous object")
          }
        case _ => fail("No TypeDecl node for anonymous object found")
      }
    }
  }

  "an anonymous type with custom type members" should {
    val cpg = code("""
        |namespace Foo {
        | public class Qux {}
        | public class Bar {
        |   public static void Main() {
        |     var q = new Qux();
        |     var Fred = new { MBar = 10, q };
        |   }
        | }
        |
        |}
        |""".stripMargin)

    "create a TypeDecl node" in {
      inside(cpg.method("Main").astChildren.isTypeDecl.l) {
        case anonType :: Nil =>
          anonType.fullName shouldBe "Foo.Bar.Main.<anon>0"
          anonType.astParentType shouldBe "METHOD"
          anonType.astParentFullName shouldBe "Foo.Bar.Main:System.Void()"
        case _ => fail("No TypeDecl node for anonymous object found")
      }
    }

    "propagate type to the LHS" in {
      inside(cpg.method("Main").astChildren.isBlock.astChildren.isLocal.nameExact("Fred").l) { case loc :: Nil =>
        loc.typeFullName shouldBe "Foo.Bar.Main.<anon>0"
      }
    }

    "have correct members" in {
      inside(cpg.method("Main").astChildren.isTypeDecl.l) {
        case anonType :: Nil =>
          inside(anonType.astChildren.isMember.l) {
            case bar :: q :: Nil =>
              bar.code shouldBe "MBar = 10"
              q.code shouldBe "q"

              bar.typeFullName shouldBe "System.Int32"
              q.typeFullName shouldBe "Foo.Qux"

              bar.astParent shouldBe anonType
              q.astParent shouldBe anonType
            case _ => fail("There should be exactly 2 members inside the anonymous object")
          }
        case _ => fail("No TypeDecl node for anonymous object found")
      }
    }
  }

  "multiple anonymous types" should {
    val cpg = code(basicBoilerplate("""
          |var Foo = new { Bar = 10, Baz = "Hello, World" };
          |var Qux = new { Fred = 5 };
          |""".stripMargin))

    "have correct attributes" in {
      inside(cpg.method("Main").astChildren.isTypeDecl.l) {
        case anonType :: anonType2 :: Nil =>
          anonType.fullName shouldBe "HelloWorld.Program.Main.<anon>0"
          anonType.astParentType shouldBe "METHOD"
          anonType.astParentFullName shouldBe "HelloWorld.Program.Main:System.Void(System.String[])"

          anonType2.fullName shouldBe "HelloWorld.Program.Main.<anon>1"
          anonType2.astParentType shouldBe "METHOD"
          anonType2.astParentFullName shouldBe "HelloWorld.Program.Main:System.Void(System.String[])"
        case _ => fail("There should be exactly 2 anonymous types present")
      }
    }

    "propagate type to the LHS" in {
      inside(cpg.method("Main").astChildren.isBlock.astChildren.isLocal.l) {
        case loc :: loc2 :: Nil =>
          loc.typeFullName shouldBe "HelloWorld.Program.Main.<anon>0"
          loc2.typeFullName shouldBe "HelloWorld.Program.Main.<anon>1"
        case _ => fail("Exactly two locals should be present")
      }
    }
  }

}
