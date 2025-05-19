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
    scope.testResolve("foo") {
      case PhpFunction(name) if name == "foo" => succeed
      case symbol                             => fail(s"Unable to resolve correct symbol (given $symbol)")
    }
  }

  "a nested function in the summary should resolve when imported" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpFunction("Foo\\foo"))
    scope.imporT("Foo\\foo")
    scope.testResolve("foo") {
      case PhpFunction(name) if name == "Foo\\foo" => succeed
      case symbol                                  => fail(s"Unable to resolve correct symbol (given $symbol)")
    }
  }

  "a nested function in the summary should resolve when imported via alias" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpFunction("Foo\\foo"))
    scope.imporT("Foo\\foo" `as` "bar")
    scope.testResolve("bar") {
      case PhpFunction(name) if name == "Foo\\foo" => succeed
      case symbol                                  => fail(s"Unable to resolve correct symbol (given $symbol)")
    }
  }

  "a nested class in the summary should resolve when imported via alias" in {
    val scope = scopeFrom(PhpNamespace("Foo"), PhpNamespace("Foo\\Bar"), PhpClass("Foo\\Bar\\C1"))
    scope.imporT("Foo\\Bar\\C1" `as` "Class1")
    scope.testResolve("Class1") {
      case PhpClass(name) if name == "Foo\\Bar\\C1" => succeed
      case symbol                                   => fail(s"Unable to resolve correct symbol (given $symbol)")
    }
  }

  "an external symbol that is not in scope" in {
    val scope = scopeFrom(PhpNamespace("Foo"))
    scope.imporT("Bar")
    scope.testResolveFailure("Bar")
  }

}

object PhpScopeTests {

  def scopeFrom(symbols: SymbolSummary*): Scope = {
    val summary = symbols.groupBy(_.name)
    Scope(summary)(() => "nan")
  }

  implicit class StringImportExt(path: String) {

    def as(alias: String): NewImport = NewImport().importedEntity(path).importedAs(alias)

  }

  implicit class ScopeExt(scope: Scope) {

    def imporT(path: String): Scope = {
      scope.useImport(NewImport().importedEntity(path) :: Nil)
      scope
    }

    def imporT(imp: NewImport): Scope = {
      scope.useImport(imp :: Nil)
      scope
    }

    def testResolve(symbol: String)(expectation: SymbolSummary => Assertion): Assertion = {
      scope.resolveImportedSymbol(symbol) match {
        case Some(symbol) => expectation(symbol)
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
