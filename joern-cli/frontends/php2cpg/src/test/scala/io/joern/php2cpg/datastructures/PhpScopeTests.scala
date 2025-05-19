package io.joern.php2cpg.datastructures

import io.joern.php2cpg.passes.SymbolSummaryPass
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.php2cpg.utils.Scope
import io.shiftleft.codepropertygraph.generated.nodes.NewImport
import org.scalatest.Assertion
import org.scalatest.Assertions.fail
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
    val scope = scopeFrom(PhpNamespace("Foo", PhpFunction("bar"):: Nil))
    scope.imporT("Foo\\foo")
    scope.testResolve("foo") {
      case PhpFunction(name) if name == "foo" => succeed
      case symbol => fail(s"Unable to resolve correct symbol (given $symbol)")
    }
  }

}

object PhpScopeTests {

  def scopeFrom(symbols: SymbolSummary*): Scope = {
    val summary = SymbolSummaryPass.globalNamespace(symbols) :: Nil
    Scope(summary)(() => "nan")
  }

  implicit class ScopeExt(scope: Scope) {

    def imporT(path: String): Scope = {
      scope.useImport(NewImport().importedEntity(path) :: Nil)
      scope
    }

    def testResolve(symbol: String)(expectation: SymbolSummary => Assertion): Assertion = {
      scope.resolveImportedSymbol(symbol) match {
        case Some(symbol) => expectation(symbol)
        case None         => fail("Unable to resolve imported symbol")
      }
    }

  }
}
