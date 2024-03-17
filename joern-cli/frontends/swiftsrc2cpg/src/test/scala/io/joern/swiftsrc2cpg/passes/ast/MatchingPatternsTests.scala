// This test file has been translated from swift/test/Parse/matching_patterns.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class MatchingPatternsTests extends AstSwiftSrc2CpgSuite {

  "MatchingPatternsTests" should {

    "testMatchingPatterns6" ignore {
      val cpg = code("""
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
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns7" ignore {
      val cpg = code("""
      |switch (x,x) {
      |  case (var a, var a):
      |  fallthrough
      |  case _:
      |  ()
      |  }
      |  """.stripMargin)
      ???
    }

    "testMatchingPatterns9" ignore {
      val cpg = code("""
      |switch e {
      |  // 'is' pattern.
      |  case is Int,
      |   is A<Int>,
      |   is A<Int>.C<Int>,
      |   is (Int, Int),
      |   is (a: Int, b: Int):
      |   ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns10" ignore {
      val cpg = code("enum Foo { case A, B, C }")
      ???
    }

    "testMatchingPatterns12" ignore {
      val cpg = code("""
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
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns13" ignore {
      val cpg = code("""
      |var n : Voluntary<Int> = .Naught
      |if case let .Naught(value) = n {}
      |if case let .Naught(value1, value2, value3) = n {}
      |if case inout .Naught(value) = n {}
      |if case _mutating .Naught(value) = n {}
      |if case _borrowing .Naught(value) = n {}
      |if case _consuming .Naught(value) = n {}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns14" ignore {
      val cpg = code("""
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
      |  """.stripMargin)
      ???
    }

    "testMatchingPatterns16" ignore {
      val cpg = code("""
      |switch notAnEnum {
      |  case .Foo:
      |  ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns19" ignore { val cpg = code("var m : ImportedEnum = .Simple") }

    "testMatchingPatterns22" ignore {
      val cpg = code("""
      |enum LabeledScalarPayload {
      |  case Payload(name: Int)
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns28" ignore { val cpg = code("var t = (1, 2, 3)") }

    "testMatchingPatterns32" ignore {
      val cpg = code("""
      |switch [Derived(), Derived(), Base()] {
      |  case let ds as [Derived]:
      |  ()
      |  case is [Derived]:
      |  ()
      |  default:
      |  ()
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns33" ignore {
      val cpg = code("""
      |// Optional patterns.
      |let op1 : Int?
      |let op2 : Int??
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns34" ignore {
      val cpg = code("""
      |switch op1 {
      |  case nil: break
      |  case 1?: break
      |  case _?: break
      |}
      |""".stripMargin)
      ???
    }

    "testMatchingPatterns35" ignore {
      val cpg = code("""
      |switch op2 {
      |  case nil: break
      |  case _?: break
      |  case (1?)?: break
      |  case (_?)?: break
      |}
      |""".stripMargin)
      ???
    }

  }

}
