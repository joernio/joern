package io.joern.x2cpg.datastructures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew
import scala.collection.mutable
import org.scalatest.Inside

class ProgramSummaryTests extends AnyWordSpec with Matchers with Inside {

  /* // Reference for the summary

    package io.joern;

    public class Foo {
        int bar(int x, int y) ...

        int bar(int x) ...
    }

   */

  val mockTyp = Typ(
    "io.joern.Foo",
    List(Method("bar", List(("x", "int"), ("y", "int")), "int"), Method("bar", List(("x", "int")), "int")),
    List.empty
  )

  val mockSummary = SummaryImpl(Map("io.joern" -> Set(mockTyp)))

  "a typed program summary based off of Java" should {

    "provide the types within a given namespace" in {
      inside(mockSummary.typesUnderNamespace("io.joern").toList) {
        case typ :: Nil =>
          typ.name shouldBe "io.joern.Foo"
        case Nil =>
          fail("Unable to resolve the types for the given namespace!")
        case _ => fail("Unexpected number of types for the given namespace")
      }
    }

    "return the associated namespace given a type" in {
      mockSummary.namespaceFor(mockTyp) match
        case None            => fail("Unable to resolve namespace!")
        case Some(namespace) => namespace shouldBe "io.joern"
    }

  }

  "a typed scope" should {

    "not be able to resolve any types with no entries" in {
      val mockScope = DefaultTypedScope(summary = mockSummary)
      mockScope.size shouldBe 0
      mockScope.tryResolveTypeReference("Foo").isDefined shouldBe false
    }

    "successfully resolve a type once the namespace scope is pushed" in {
      val mockScope = DefaultTypedScope(summary = mockSummary)
      mockScope.pushNewScope(NamespaceTSEImpl("io.joern"))
      mockScope.tryResolveTypeReference("Foo") match
        case None      => fail("Unable to resolve type!")
        case Some(typ) => typ.name shouldBe "io.joern.Foo"
    }

    "unable to resolve a type once the namespace scope is popped and pushed off again" in {
      val mockScope = DefaultTypedScope(summary = mockSummary)
      mockScope.pushNewScope(NamespaceTSEImpl("io.joern"))
      mockScope.popScope()
      mockScope.tryResolveTypeReference("Foo") match
        case None      => // correct behaviour
        case Some(typ) => fail("Type should no longer be on the stack!")
    }

  }

  class SummaryImpl(initMap: Map[String, Set[Typ]]) extends ProgramSummary[Method, Field, Typ] {
    override protected val namespaceToType = Map.from(initMap)
  }

  case class NamespaceTSEImpl(fullName: String) extends NamespaceTSE

  case class Method(name: String, parameterTypes: List[(String, String)], returnType: String) extends MethodLike

  case class Field(name: String, typeName: String) extends FieldLike

  case class Typ(name: String, methods: List[Method], fields: List[Field]) extends TypeLike[Method, Field]

}
