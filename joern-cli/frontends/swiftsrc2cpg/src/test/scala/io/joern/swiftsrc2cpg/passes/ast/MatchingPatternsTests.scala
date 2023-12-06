// This test file has been translated from swift/test/Parse/matching_patterns.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MatchingPatternsTests extends AbstractPassTest {

  "MatchingPatternsTests" should {

    "testMatchingPatterns6" ignore AstFixture("""
      |switch x {
      |  // Expressions as patterns.
      |  case 0:
      |  ()
      |  case 1 + 2:
      |  ()
      |  case square(9):
      |  ()
      |  // 'var' and 'let' patterns.
      |  case var a:
      |  a = 1
      |  case let a:
      |  a = 1
      |  case inout a:
      |  a = 1
      |  case var var a:
      |  a += 1
      |  case var let a:
      |  print(a, terminator: "")
      |  case var (var b):
      |  b += 1
      |  // 'Any' pattern.
      |  case _:
      |  ()
      |  // patterns are resolved in expression-only positions are errors.
      |  case 1 + (_):
      |  ()
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns7" ignore AstFixture("""
      |switch (x,x) {
      |  case (var a, var a):
      |  fallthrough
      |  case _:
      |  ()
      |  }
      |  """.stripMargin) { cpg => }

    "testMatchingPatterns9" ignore AstFixture("""
      |switch e {
      |  // 'is' pattern.
      |  case is Int,
      |   is A<Int>,
      |   is A<Int>.C<Int>,
      |   is (Int, Int),
      |   is (a: Int, b: Int):
      |   ()
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns10" ignore AstFixture("enum Foo { case A, B, C }") { cpg => }

    "testMatchingPatterns12" ignore AstFixture("""
      |enum Voluntary<T> : Equatable {
      |  case Naught
      |  case Mere(T)
      |  case Twain(T, T)
      |  func enumMethod(_ other: Voluntary<T>, foo: Foo) {
      |  switch self {
      |  case other:
      |  ()
      |  case .Naught,
      |   .Naught(),
      |   .Naught(_),
      |   .Naught(_, _):
      |  ()
      |  case .Mere,
      |   .Mere(),
      |   .Mere(_),
      |   .Mere(_, _):
      |  ()
      |  case .Twain(),
      |   .Twain(_),
      |   .Twain(_, _),
      |   .Twain(_, _, _):
      |  ()
      |  }
      |  switch foo {
      |  case .Naught:
      |  ()
      |  case .A, .B, .C:
      |  ()
      |  }
      |  }
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns13" ignore AstFixture("""
      |var n : Voluntary<Int> = .Naught
      |if case let .Naught(value) = n {}
      |if case let .Naught(value1, value2, value3) = n {}
      |if case inout .Naught(value) = n {}
      |if case _mutating .Naught(value) = n {}
      |if case _borrowing .Naught(value) = n {}
      |if case _consuming .Naught(value) = n {}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns14" ignore AstFixture("""
      |switch n {
      |  case Foo.A:
      |  ()
      |  case Voluntary<Int>.Naught,
      |   Voluntary<Int>.Naught(),
      |   Voluntary<Int>.Naught(_, _),
      |   Voluntary.Naught,
      |   .Naught:
      |  ()
      |  case Voluntary<Int>.Mere,
      |   Voluntary<Int>.Mere(_),
      |   Voluntary<Int>.Mere(_, _),
      |   Voluntary.Mere,
      |   Voluntary.Mere(_),
      |   .Mere,
      |   .Mere(_):
      |  ()
      |  case .Twain,
      |   .Twain(_),
      |   .Twain(_, _),
      |   .Twain(_, _, _):
      |  ()
      |  }
      |  """.stripMargin) { cpg => }

    "testMatchingPatterns16" ignore AstFixture("""
      |switch notAnEnum {
      |  case .Foo:
      |  ()
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns19" ignore AstFixture("var m : ImportedEnum = .Simple") { cpg => }

    "testMatchingPatterns22" ignore AstFixture("""
      |enum LabeledScalarPayload {
      |  case Payload(name: Int)
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns28" ignore AstFixture("var t = (1, 2, 3)") { cpg => }

    "testMatchingPatterns32" ignore AstFixture("""
      |switch [Derived(), Derived(), Base()] {
      |  case let ds as [Derived]:
      |  ()
      |  case is [Derived]:
      |  ()
      |  default:
      |  ()
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns33" ignore AstFixture("""
      |// Optional patterns.
      |let op1 : Int?
      |let op2 : Int??
      |""".stripMargin) { cpg => }

    "testMatchingPatterns34" ignore AstFixture("""
      |switch op1 {
      |  case nil: break
      |  case 1?: break
      |  case _?: break
      |}
      |""".stripMargin) { cpg => }

    "testMatchingPatterns35" ignore AstFixture("""
      |switch op2 {
      |  case nil: break
      |  case _?: break
      |  case (1?)?: break
      |  case (_?)?: break
      |}
      |""".stripMargin) { cpg => }

  }

}
