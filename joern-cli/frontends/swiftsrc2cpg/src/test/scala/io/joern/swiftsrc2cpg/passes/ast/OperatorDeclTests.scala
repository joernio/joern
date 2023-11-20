

// This test file has been translated from swift/test/Parse/operator_decl.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class OperatorDeclTests extends AbstractPassTest {
  "testOperatorDecl1a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator +++ 1️⃣{}
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: "prefix operator +++"
    )
  }

  "testOperatorDecl1b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator +++ 1️⃣{}
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        postfix operator +++
        """
    )
  }

  "testOperatorDecl1c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator +++ 1️⃣{}
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        infix operator +++
        """
    )
  }

  "testOperatorDecl1d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator +++* 1️⃣{
        associativity right
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        infix operator +++*
        """
    )
  }

  "testOperatorDecl1e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator +++*+ : A 1️⃣{ }
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        infix operator +++*+ : A
        """
    )
  }

  "testOperatorDecl2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator +++** : A 1️⃣{ }
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        prefix operator +++** : A
        """
    )
  }

  "testOperatorDecl3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator ++*++ : A
      """
    )
  }

  "testOperatorDecl4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator ++*+* : A 1️⃣{ }
      """,
      diagnostics: [
        DiagnosticSpec(message: "operator should not be declared with body", fixIts: ["remove operator body"])
      ],
      fixedSource: """
        postfix operator ++*+* : A
        """
    )
  }

  "testOperatorDecl5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator ++**+ : A
      """
    )
  }

  "testOperatorDecl6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣operator ++*** : A
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "operator must be declared as 'prefix', 'postfix', or 'infix'",
          fixIts: ["insert 'prefix'", "insert 'infix'", "insert 'postfix'"]
        )
      ],
      fixedSource:
        """
        prefix operator ++*** : A
        """
    )
  }

  "testOperatorDecl7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣operator +*+++ 2️⃣{ }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "operator must be declared as 'prefix', 'postfix', or 'infix'",
          fixIts: ["insert 'prefix'", "insert 'infix'", "insert 'postfix'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "operator should not be declared with body", fixIts: ["remove operator body"]),
      ],
      fixedSource: """
        prefix operator +*+++
        """
    )
  }

  "testOperatorDecl8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣operator +*++* : A 2️⃣{ }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "operator must be declared as 'prefix', 'postfix', or 'infix'",
          fixIts: ["insert 'prefix'", "insert 'infix'", "insert 'postfix'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "operator should not be declared with body", fixIts: ["remove operator body"]),
      ],
      fixedSource: """
        prefix operator +*++* : A
        """
    )
  }

  "testOperatorDecl9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected binary operator in operator declaration", fixIts: ["insert binary operator"])
      ],
      fixedSource: """
        prefix operator <#binary operator#>
        """
    )
  }

  "testOperatorDecl10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣;
      prefix operator %%+
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected ';' separator", fixIts: ["remove ';'"])
      ],
      fixedSource:
        """

        prefix operator %%+
        """
    )
  }

  "testOperatorDecl11a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator ??
      """
    )
  }

  "testOperatorDecl11b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator ??
      """
    )
  }

  "testOperatorDecl11c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator !!
      """
    )
  }

  "testOperatorDecl11d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator !!
      """
    )
  }

  "testOperatorDecl11e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator ?1️⃣$$
      """,
      diagnostics: [
        DiagnosticSpec(message: "'$$' is considered an identifier and must not appear within an operator name", fixIts: ["remove '$$'"])
      ],
      fixedSource: "postfix operator ?"
    )
  }

  "testOperatorDecl12a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator --1️⃣aa
      """,
      diagnostics: [
        DiagnosticSpec(message: "'aa' is considered an identifier and must not appear within an operator name", fixIts: ["remove 'aa'"])
      ],
      fixedSource: "infix operator --"
    )
  }

  "testOperatorDecl12b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator 1️⃣aa--: A
      """,
      diagnostics: [
        DiagnosticSpec(message: "'aa' is considered an identifier and must not appear within an operator name", highlight: "aa", fixIts: ["remove 'aa'"])
      ],
      fixedSource: "infix operator --: A"
    )
  }

  "testOperatorDecl12c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator <<1️⃣$$@<
      """,
      diagnostics: [
        DiagnosticSpec(message: "'$$@<' is not allowed in operator names", fixIts: ["remove '$$@<'"])
      ],
      fixedSource: "infix operator <<"
    )
  }

  "testOperatorDecl12d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator !!1️⃣@aa2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "'@aa' is not allowed in operator names", fixIts: ["remove '@aa'"])
      ],
      fixedSource: """
        infix operator !!
        """
    )
  }

  "testOperatorDecl12e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator 1️⃣#++=
      """,
      diagnostics: [
        DiagnosticSpec(message: "'#' is not allowed in operator names", highlight: "#", fixIts: ["remove '#'"])
      ],
      fixedSource: "infix operator ++="
    )
  }

  "testOperatorDecl12f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator ++=1️⃣#
      """,
      diagnostics: [
        DiagnosticSpec(message: "'#' is not allowed in operator names", fixIts: ["remove '#'"])
      ],
      fixedSource: """
        infix operator ++=
        """
    )
  }

  "testOperatorDecl12g" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator ->1️⃣#
      """,
      diagnostics: [
        DiagnosticSpec(message: "'#' is not allowed in operator names", fixIts: ["remove '#'"])
      ],
      fixedSource: """
        infix operator ->
        """
    )
  }

  "testOperatorDecl13" ignore AstFixture("") { cpg =>
    // FIXME: Ideally, we shouldn't emit the «consistent whitespace» diagnostic
    // where = cannot possibly mean an assignment.
    assertParse(
      """
      infix operator =1️⃣#=
      """,
      diagnostics: [
        DiagnosticSpec(message: "'#=' is not allowed in operator names", fixIts: ["remove '#='"])
      ],
      fixedSource: """
        infix operator =
        """
    )
  }

  "testOperatorDecl14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator +++=
      infix operator *** : A
      infix operator --- : 1️⃣;
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected precedence group in operator declaration", fixIts: ["insert precedence group"])
      ],
      fixedSource: """
        infix operator +++=
        infix operator *** : A
        infix operator --- : <#identifier#>;
        """
    )
  }

  "testOperatorDecl15a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup 1️⃣{
        associativity: right
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in precedencegroup", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        precedencegroup <#identifier#> {
          associativity: right
        }
        """
    )
  }

  "testOperatorDecl15b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup A {
        associativity 1️⃣right
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in 'associativity' property of precedencegroup", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        precedencegroup A {
          associativity: right
        }
        """
    )
  }

  "testOperatorDecl15c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup B {
        1️⃣precedence 123
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'precedence 123' in precedencegroup")
      ]
    )
  }

  "testOperatorDecl15d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup C {
        associativity: 1️⃣sinister
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "Expected 'none', 'left', or 'right' after 'associativity'")
      ]
    )
  }

  "testOperatorDecl15e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup D {
        assignment: 1️⃣no
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'true' or 'false' after 'assignment'")
      ]
    )
  }

  "testOperatorDecl15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup E {
        higherThan:1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected name in 'relation' property of precedencegroup", fixIts: ["insert name"])
      ],
      fixedSource: """
        precedencegroup E {
          higherThan: <#identifier#>
        }
        """
    )
  }

  "testOperatorDecl16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup F {
        higherThan: A, B, C
      }
      """
    )
  }

  "testOperatorDecl17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup BangBangBang {
        associativity: none
        associativity: left
      }
      """
    )
  }

  "testOperatorDecl18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup CaretCaretCaret {
        assignment: true
        assignment: false
      }
      """
    )
  }

  "testOperatorDecl19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Foo {
        infix operator |||
      }
      """
    )
  }

  "testOperatorDecl20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator **<< : UndeclaredPrecedenceGroup
      """
    )
  }

  "testOperatorDecl21" ignore AstFixture("") { cpg =>
    // TODO: We should not allow specification of multiple precedence groups
    assertParse(
      """
      protocol Proto {}
      infix operator *<*< : F, Proto
      """
    )
  }

  "testOperatorDecl22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/60932
      """
    )
  }

  "testOperatorDecl23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator ++:1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected precedence group in operator declaration", fixIts: ["insert precedence group"])
      ],
      fixedSource: """
        postfix operator ++: <#identifier#>
        """
    )
  }

  "testOperatorDecl124" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣@objc
      postfix operator ++: PrecedenceGroup
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '@objc' before operator declaration")
      ]
    )
  }

  "testOperatorDecl125" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣mutating postfix operator --: UndefinedPrecedenceGroup
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'mutating' before operator declaration")
      ]
    )
  }

  "testOperatorDecl126" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣private(set) infix operator ~~~
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'private(set)' before operator declaration")
      ]
    )
  }

  "testOperatorDecl127" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣dynamic 2️⃣operator ~~~
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'dynamic' before operator declaration"),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "operator must be declared as 'prefix', 'postfix', or 'infix'",
          fixIts: ["insert 'prefix'", "insert 'infix'", "insert 'postfix'"]
        ),
      ],
      fixedSource:
        """
        dynamic prefix operator ~~~
        """
    )
  }

  "testMultipleFixity" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix 1️⃣infix operator &+&
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'infix' in operator declaration")
      ]
    )
  }

  "testIdentifierAsOperatorName" ignore AstFixture("") { cpg =>
    assertParse(
      "postfix operator 1️⃣aa",
      diagnostics: [
        DiagnosticSpec(message: "'aa' is considered an identifier and must not appear within an operator name")
      ]
    )
  }

  "testRegexLikeOperator" ignore AstFixture("") { cpg =>
    assertParse("prefix operator /^/")
  }
}
