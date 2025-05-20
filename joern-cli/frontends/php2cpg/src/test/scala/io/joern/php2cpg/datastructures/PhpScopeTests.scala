package io.joern.php2cpg.datastructures

import io.joern.php2cpg.passes.SymbolSummaryPass
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.php2cpg.utils.Scope
import io.shiftleft.codepropertygraph.generated.nodes.NewImport
import org.scalatest.Assertion
import org.scalatest.Assertions.{fail, succeed}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PhpScopeTests extends AnyWordSpec with Matchers {

  import PhpScopeTests.*

  "a function in the summary should resolve when imported" in {
    val scope = scopeFrom(PhpFunction("foo"))
    scope.imporT("foo")
    scope.testResolve[PhpFunction]("foo")(_.name shouldBe "foo")
  }

  "a nested function in the summary should resolve when imported" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpFunction("Foo\\foo"))
    scope.imporT("Foo\\foo")
    scope.testResolve[PhpFunction]("foo")(_.name shouldBe "Foo\\foo")
  }

  "a nested function in the summary should resolve when imported via alias" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpFunction("Foo\\foo"))
    scope.imporT("Foo\\foo" `as` "bar")
    scope.testResolve[PhpFunction]("bar")(_.name shouldBe "Foo\\foo")
  }

  "a nested class in the summary should resolve when imported via alias" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpNamespace("Foo\\Bar"), PhpClass("Foo\\Bar\\C1"))
    scope.imporT("Foo\\Bar\\C1" `as` "Class1")
    scope.testResolve[PhpClass]("Class1")(_.name shouldBe "Foo\\Bar\\C1")
  }

  "an external symbol that is not in scope" in {
    val scope = scopeFrom(PhpNamespace("Foo"))
    scope.imporT("Bar")
    scope.testResolveFailure("Bar")
  }

  "when namespace and class share the same path, the class should be selected over the namespace" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpClass("Foo"))
    scope.imporT("Foo")
    scope.testResolve[PhpClass]("Foo")(_.name shouldBe "Foo")
  }

  "when namespace, class, and function share the same path, the class should be selected over the namespace" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpClass("Foo"), PhpFunction("Foo"))
    scope.imporT("Foo")
    scope.testResolve[PhpClass]("Foo")(_.name shouldBe "Foo")
  }

  "when namespace and function share the same path, the function should be selected over the namespace" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpFunction("Foo"))
    scope.imporT("Foo")
    scope.testResolve[PhpFunction]("Foo")(_.name shouldBe "Foo")
  }

}

object PhpScopeTests {

  def scopeFrom(symbols: SymbolSummary*): Scope = {
    val summary = symbols.groupBy(_.name)
    Scope(summary)(() => "nan")
  }

  extension (path: String) {

    def as(alias: String): NewImport = NewImport().importedEntity(path).importedAs(alias)

  }

  extension (scope: Scope) {

    def imporT(path: String): Scope = {
      scope.useImport(NewImport().importedEntity(path) :: Nil)
      scope
    }

    def imporT(imp: NewImport): Scope = {
      scope.useImport(imp :: Nil)
      scope
    }

    def testResolve[T <: SymbolSummary](symbol: String)(expectation: T => Assertion): Assertion = {
      scope.resolveImportedSymbol(symbol) match {
        case Some(symbol) => try {
          expectation(symbol.asInstanceOf[T])
        } catch {
          case _: Exception => fail(s"Unable to resolve correct symbol (given $symbol)")
        }
        case None         => fail("Unable to resolve imported symbol")
      }
    }

    def testResolveFailure(symbol: String): Assertion = {
      scope.resolveImportedSymbol(symbol) match {
        case Some(symbol) => fail(s"Incorrectly resolved '$symbol'")
        case None         => succeed
      }
    }

  }
}
