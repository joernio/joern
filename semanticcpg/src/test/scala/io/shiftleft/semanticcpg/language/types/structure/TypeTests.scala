package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeTests extends AnyWordSpec with Matchers {

  "ClassMemberTest" should {

    val cpg = MockCpg()
      .withTypeDecl("ClassMemberTest", isExternal = false)
      .withTypeDecl("Object", isExternal = true)
      .cpg

    "have ClassMemberTest as internal class" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl.name("ClassMemberTest").internal.toList
      queryResult.size shouldBe 1
    }

    "have Object as external class" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl.name("Object").external.toList
      queryResult.size shouldBe 1
    }

    "have a member called member" in {
      def queryResult: List[Member] =
        cpg.typeDecl("ClassMemberTest").member.nameExact("amember").toList
      queryResult.size shouldBe 1
    }

    "have a static member called amember" in {
      def queryResult: List[Member] =
        cpg.typeDecl("ClassMemberTest").member.nameExact("amember").isStatic.toList
      queryResult.size shouldBe 1
    }

    "more than 0 members found by regex" in {
      def queryResult: List[Member] =
        cpg.typeDecl.member.name(".*").toList

      queryResult.size should be > 0
    }
  }

  "ClassHierarchyTest for type declarations" should {

    val cpg = MockCpg()
      .withTypeDecl("ClassMemberTest", isExternal = false)
      .withTypeDecl("Object", isExternal = true)
      .withTypeDecl("Base")
      .withTypeDecl("Derived")
      .withTypeDecl("DerivedFromDerived")
      .withCustom { case (graph, cpg) =>
        graph.addEdge(cpg.typeDecl("Derived").head, cpg.typ.name("Base").head, EdgeTypes.INHERITS_FROM)
        graph.addEdge(cpg.typeDecl("DerivedFromDerived").head, cpg.typ.name("Derived").head, EdgeTypes.INHERITS_FROM)
      }
      .cpg

    "have class Base as base class of class Derived" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl
          .name(".*Derived")
          .baseTypeDecl
          .name(".*Base")
          .toList

      queryResult.size shouldBe 1
    }

    "have class Dervied as derived class of class Base" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl
          .name(".*Base")
          .derivedTypeDecl
          .name(".*Derived")
          .toList

      queryResult.size shouldBe 1
    }

    "have Derived and DerivedFromDerived as transitive derived types of Base" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl
          .name(".*Base")
          .derivedTypeDeclTransitive
          .toList

      queryResult.map(_.name).toSet shouldBe Set("Derived", "DerivedFromDerived")
    }

    "have Base and Object as transitive base types of Derived" in {
      def queryResult: List[TypeDecl] =
        cpg.typeDecl
          .name("DerivedFromDerived")
          .baseTypeDeclTransitive
          .toList

      queryResult.map(_.name).toSet shouldBe Set("Derived", "Base")
    }
  }

  "ClassHierarchyTest for types" should {

    val cpg = MockCpg()
      .withTypeDecl("ClassMemberTest", isExternal = false)
      .withTypeDecl("Object", isExternal = true)
      .withTypeDecl("Base")
      .withTypeDecl("Derived")
      .withTypeDecl("DerivedFromDerived")
      .withCustom { case (graph, cpg) =>
        graph.addEdge(cpg.typeDecl("Derived").head, cpg.typ.name("Base").head, EdgeTypes.INHERITS_FROM)
        graph.addEdge(cpg.typeDecl("DerivedFromDerived").head, cpg.typ.name("Derived").head, EdgeTypes.INHERITS_FROM)
      }
      .cpg

    "have class Base as base class of class Derived" in {
      def queryResult: List[Type] =
        cpg.typ
          .name(".*Derived")
          .baseType
          .name(".*Base")
          .toList

      queryResult.size shouldBe 1
    }

    "have class Dervied as derived class of class Base" in {
      def queryResult: List[Type] =
        cpg.typ
          .name(".*Base")
          .derivedType
          .name(".*Derived")
          .toList

      queryResult.size shouldBe 1
    }

    "have Derived and DerivedFromDerived as transitive derived types of Base" in {
      def queryResult: List[Type] =
        cpg.typ
          .name(".*Base")
          .derivedTypeTransitive
          .toList

      queryResult.map(_.name).toSet shouldBe Set("Derived", "DerivedFromDerived")
    }

    "have Base and Object as transitive base types of Derived" in {
      def queryResult: List[Type] =
        cpg.typ
          .name("DerivedFromDerived")
          .baseTypeTransitive
          .toList

      queryResult.map(_.name).toSet shouldBe Set("Derived", "Base")
    }

  }

  "Class content test" should {
    val cpg = MockCpg()
      .withNamespace("namespace")
      .withFile("someFile", Some("aaaCONTENTbbb"))
      .withTypeDecl(
        "foo",
        inNamespace = Some("namespace"),
        inFile = Some("someFile"),
        offset = Some(3),
        offsetEnd = Some(10)
      )
      .cpg

    "have correct content for class" in {
      cpg.typeDecl.name("foo").content.head shouldBe "CONTENT"
    }
  }
}
