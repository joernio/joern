

import SwiftParser
import SwiftSyntax
import SwiftSyntaxBuilder
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

/// Test ``StringLiteralExprSyntax/representedLiteralValue``.
///
/// Most tests are expressed by a single function call that compares the
/// run-time String value against the parsed node's `representedLiteralValue`.
public class StringLiteralRepresentedLiteralValueTests: ParserTestCase {

  "testIntro" ignore AstFixture("") { cpg =>
    test(
      #"""
      This test suite verifies the correctness of ``StringLiteralExprSyntax/\
      representedLiteralValue.`` It does so by comparing the run-time String
      value of various string literals to the value returned from
      `representedLiteralValue`.

      The SwiftSyntax parsed representation `StringLiteralExprSyntax` contains
      the source-accurate representation of the string literal. This
      representation differs from the runtime value:

        - Escaped character sequences (\n, \u{1F600}) are not evaluated.
        - Some strings are split into segments (i.e. multiline strings).

      The idea of `representedLiteralValue` is to create the run-time value of a
      string literal during parse time. Obviously this is only possible for
      static Strings so interpolated strings do not work.
      """#
    )
  }

  // MARK: double quoted string literal

  "testDoubleQuoted_emptyString" ignore AstFixture("") { cpg =>
    test("")
  }

  "testDoubleQuoted_singleSpace" ignore AstFixture("") { cpg =>
    test(" ")
  }

  "testDoubleQuoted_singleQuote" ignore AstFixture("") { cpg =>
    test("'")
  }

  "testDoubleQuoted_emoji" ignore AstFixture("") { cpg =>
    test("😀")
  }

  "testDoubleQuoted_escapedNul" ignore AstFixture("") { cpg =>
    test("\0")
  }

  "testDoubleQuoted_escapedNL" ignore AstFixture("") { cpg =>
    test("\n")
  }

  "testDoubleQuoted_escapedCR" ignore AstFixture("") { cpg =>
    test("\r")
  }

  "testDoubleQuoted_escapedTab" ignore AstFixture("") { cpg =>
    test("\t")
  }

  "testDoubleQuoted_escapedDoubleQuote" ignore AstFixture("") { cpg =>
    test("\"")
  }

  "testDoubleQuoted_escapedSingleQuote" ignore AstFixture("") { cpg =>
    test("\'")
  }

  "testDoubleQuoted_escapedEscape" ignore AstFixture("") { cpg =>
    test("\\")
  }

  "testDoubleQuoted_escapedUnicodeDot" ignore AstFixture("") { cpg =>
    test("\u{2e}")
  }

  "testDoubleQuoted_escapedEmoji" ignore AstFixture("") { cpg =>
    test("\u{1F600}")
  }

  // MARK: raw double quoted string literal

  "testRawDoubleQuoted_emptyString" ignore AstFixture("") { cpg =>
    test(#""#)
  }

  "testRawDoubleQuoted_singleSpace" ignore AstFixture("") { cpg =>
    test(#" "#)
  }

  "testRawDoubleQuoted_unescapedDoubleQuote" ignore AstFixture("") { cpg =>
    test(#"""#)
  }

  "testRawDoubleQuoted_unescapedBackslash" ignore AstFixture("") { cpg =>
    test(#"\"#)
  }

  "testRawDoubleQuoted_emoji" ignore AstFixture("") { cpg =>
    test(#"😀"#)
  }

  "testRawDoubleQuoted_escapedNul" ignore AstFixture("") { cpg =>
    test(#"\#0"#)
  }

  "testRawDoubleQuoted_escapedNL" ignore AstFixture("") { cpg =>
    test(#"\#n"#)
  }

  "testRawDoubleQuoted_escapedCR" ignore AstFixture("") { cpg =>
    test(#"\#r"#)
  }

  "testRawDoubleQuoted_escapedTab" ignore AstFixture("") { cpg =>
    test(#"\#t"#)
  }

  "testRawDoubleQuoted_escapedDoubleQuote" ignore AstFixture("") { cpg =>
    test(#"\#""#)
  }

  "testRawDoubleQuoted_escapedSingleQuote" ignore AstFixture("") { cpg =>
    test(#"\#'"#)
  }

  "testRawDoubleQuoted_escapedEscape" ignore AstFixture("") { cpg =>
    test(#"\#\#"#)
  }

  "testRawDoubleQuoted_escapedUnicodeDot" ignore AstFixture("") { cpg =>
    test(#"\#u{2e}"#)
  }

  "testRawDoubleQuoted_escapedEmoji" ignore AstFixture("") { cpg =>
    test(#"\#u{1F600}"#)
  }

  // MARK: multi line string literal

  "testMultiLine_emptyString" ignore AstFixture("") { cpg =>
    test(
      """

      """
    )
  }

  "testMultiLine_emptyLine" ignore AstFixture("") { cpg =>
    test(
      """


      """
    )
  }

  "testMultiLine_helloWorld" ignore AstFixture("") { cpg =>
    test(
      """
      Hello, world!
      """
    )
  }

  "testMultiLine_indentedLines" ignore AstFixture("") { cpg =>
    test(
      """
      not indented
        2 spaces indented
          4 spaces indented
      """
    )
  }

  "testMultiLine_escapedLine" ignore AstFixture("") { cpg =>
    test(
      """
      Line 1\
      .still on Line 1
      """
    )
  }

  // MARK: raw multi line string literal

  "testRawMultiLine_emptyString" ignore AstFixture("") { cpg =>
    test(
      #"""

      """#
    )
  }

  "testRawMultiLine_emptyLine" ignore AstFixture("") { cpg =>
    test(
      #"""


      """#
    )
  }

  "testRawMultiLine_helloWorld" ignore AstFixture("") { cpg =>
    test(
      #"""
      Hello, world!
      """#
    )
  }

  "testRawMultiLine_indentedLines" ignore AstFixture("") { cpg =>
    test(
      #"""
      not indented
        2 spaces indented
          4 spaces indented
      """#
    )
  }

  "testRawMultiLine_escapedLine" ignore AstFixture("") { cpg =>
    test(
      #"""
      Line 1\
      .still on Line 1
      """#
    )
  }

  // MARK: literal value not available

  func testMissingQuoteStringLiteral() throws {
    var parser = Parser(#""a"#)
    let stringLiteral = StringLiteralExprSyntax(ExprSyntax.parse(from: &parser))!
    XCTAssertNil(stringLiteral.representedLiteralValue, "only fully parsed string literals should produce a literal value")
  }

  func testInterpolatedStringLiteral() throws {
    let stringLiteral = StringLiteralExprSyntax(#""abc\(1)""# as ExprSyntax)!
    XCTAssertNil(stringLiteral.representedLiteralValue, "interpolated string literals cannot produce a literal value")
  }

  func testMalformedMultiLineStringLiteral() throws {
    var parser = Parser(#""""a""""#)
    let stringLiteral = StringLiteralExprSyntax(ExprSyntax.parse(from: &parser))!
    XCTAssertNil(stringLiteral.representedLiteralValue, "missing newline in multiline string literal cannot produce a literal value")
  }

  // MARK: supporting code

  /// This helper function takes a string literal argument and compares its
  /// value with the parsed node of type StringLiteralExprSyntax. To produce the
  /// node it parses the contents of this source file and matches the literal by
  /// position in source.
  func test(_ expected: StaticString, file: StaticString = #filePath, line: UInt = #line) {
    guard let literal = Self.literals[at: line] else {
      fatalError("string literal not found at line \(line)")
    }

    guard let representedLiteralValue = literal.representedLiteralValue else {
      XCTFail("literal unexpectedly product nil value", file: file, line: line)
      return
    }

    XCTAssertEqual(representedLiteralValue, expected.description, file: file, line: line)
  }

  static let literals = try! StringLiteralCollector()

  /// Helper class to find string literals in this source file
  class StringLiteralCollector: SyntaxVisitor {
    var locationConverter: SourceLocationConverter? = nil
    var literals: [UInt: [StringLiteralExprSyntax]] = [:]

    init(file: String = #filePath) throws {
      let url = URL(fileURLWithPath: file)
      let fileData = try Data(contentsOf: url)
      let source = fileData.withUnsafeBytes {
        String(decoding: $0.bindMemory(to: UInt8.self), as: UTF8.self)
      }
      let syntax = Parser.parse(source: source)

      super.init(viewMode: .sourceAccurate)

      self.locationConverter = SourceLocationConverter(fileName: "", tree: syntax)
      self.walk(syntax)
      self.locationConverter = nil
    }

    subscript(at line: UInt, index: Int = 0) -> StringLiteralExprSyntax? {
      literals[line]?[index]
    }

    override func visit(_ node: StringLiteralExprSyntax) -> SyntaxVisitorContinueKind {
      let line = UInt(locationConverter?.location(for: node.position).line ?? 0)
      literals[line, default: []].append(node)
      return .visitChildren
    }
  }
}
