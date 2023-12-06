// This test file has been translated from swift/test/Parse/object_literals.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ObjectLiteralsTests extends AbstractPassTest {

  "ObjectLiteralsTests" should {

    "testObjectLiterals2a" ignore AstFixture(
      "let _ = #Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)"
    ) { cpg => }

    "testObjectLiterals2b" ignore AstFixture("let _ = #Image(imageLiteral: localResourceNameAsString)") { cpg => }

    "testObjectLiterals3a" ignore AstFixture("let _ = #notAPound") { cpg => }

    "testObjectLiterals3b" ignore AstFixture("let _ = #notAPound(1, 2)") { cpg => }

    "testObjectLiterals3c" ignore AstFixture("let _ = #Color") { cpg => }

    "testObjectLiterals5" ignore AstFixture(" let _ = [#Color(_: 1, green: 1, 2)]") { cpg => }

    "testObjectLiterals8" ignore AstFixture(" let _ = #Color(_: 1, green: 1)") { cpg => }

  }

}
