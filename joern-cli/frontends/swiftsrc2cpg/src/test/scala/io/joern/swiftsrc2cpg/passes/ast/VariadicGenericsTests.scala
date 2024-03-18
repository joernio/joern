package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class VariadicGenericsTests extends AstSwiftSrc2CpgSuite {

  "VariadicGenericsTests" should {

    "testRequirement" ignore {
      val cpg = code("func requirement<each T>() where each T: P {}")
      ???
    }

    "testPackElementExprSimple" ignore {
      val cpg = code("""
        |func tuplify<each T>(_ t: repeat each T) -> (repeat each T) {
        |  return (repeat each t)
        |}
        |func zip<each T, each U>(_ first: repeat each T, with second: repeat each U) -> (repeat (each T, each U)) {
        |  return (repeat (each first, each second))
        |}
        |func variadicMap<each T, each Result>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
        |  return (repeat (each transform)(each t))
        |}
        |""".stripMargin)
      ???
    }

    "testMetatype" ignore {
      val cpg = code("G<Int, repeat Array<each T>>.self")
      ???
    }

    "testParameterPacks1" ignore {
      val cpg = code("func f1<each T>() -> repeat each T {}")
      ???
    }

    "testParameterPacks2" ignore {
      val cpg = code("func f2<each T>() -> (repeat each T) {}")
      ???
    }

    "testParameterPacks3" ignore {
      val cpg = code("func f3<each T>() -> G<repeat each T> {}")
      ???
    }

    "testParameterPacks5" ignore {
      val cpg = code("typealias Alias<each T> = (repeat each T)")
      ???
    }

    "testParameterPacks6" ignore {
      val cpg = code("struct S<each T> {}")
      ???
    }

    "testParameterPacks7" ignore {
      val cpg = code("struct S<T, each U> {}")
      ???
    }

    "testParameterPacks8" ignore {
      val cpg = code("struct S<each T, U> {}")
      ???
    }

    "testParameterPacks9" ignore {
      val cpg = code("struct S<each T:P, U> {}")
      ???
    }

    "testParameterPacks10" ignore {
      val cpg = code("struct S<each T :P, U> {}")
      ???
    }

    "testParameterPacks11" ignore {
      val cpg = code("struct S<each T: P> {}")
      ???
    }

    "testParameterPacks12" ignore {
      val cpg = code("struct S<each T : P> {}")
      ???
    }

    "testParameterPacks13" ignore {
      val cpg = code("func foo<each T>(_ x: repeat each T) {}")
      ???
    }

    "testParameterPacks14" ignore {
      val cpg = code("func foo<each T:P>(_ x: repeat each T) {}")
      ???
    }

    "testParameterPacks15" ignore {
      val cpg = code("func foo<each T :P>(_ x: repeat each T) {}")
      ???
    }

    "testParameterPacks16" ignore {
      val cpg = code("func foo<each T : P>(_ x: repeat each T) {}")
      ???
    }

    "testParameterPacks17" ignore {
      val cpg = code("func foo<each T: P>(_ x: repeat each T) {}")
      ???
    }

    "testParameterPacks18" ignore {
      val cpg = code("func foo<T, U, each V>(x: T, y: U, z: repeat each V) {}")
      ???
    }

    "testParameterPacks19" ignore {
      val cpg = code("func foo<T, each U, V>(x: T, y: repeat each U, z: V) {}")
      ???
    }

    "testParameterPacks20" ignore {
      val cpg = code("func foo<each T, each U, each V>(x: repeat each T, y: repeat each U, z: repeat each V) {}")
      ???
    }

    "testParameterPacks21" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  case f1(_: repeat each T)
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks22" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  case f2(_: G<repeat each T>)
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks23" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  var x: repeat each T { fatalError() }
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks24" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  var x: (repeat each T) { fatalError() }
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks25" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  subscript(_: repeat each T) -> Int { fatalError() }
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks26" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  subscript() -> repeat each T { fatalError() }
        |}
        |""".stripMargin)
      ???
    }

    "testParameterPacks27" ignore {
      val cpg = code("""
        |enum E<each T> {
        |  subscript() -> (repeat each T) { fatalError() }
        |}
        |""".stripMargin)
      ???
    }

    "testVariadicTypes" ignore {
      val cpg = code("""
        |let _: G< > = G()
        |let _: G<repeat each T> = G()
        |let _: G<Int, repeat each T> = G()
        |let _ = G< >.self
        |let _ = G<repeat each T>.self
        |let _ = G<Int, repeat each T>.self
        |""".stripMargin)
      ???
    }

    "testMissingCommaInType" ignore {
      val cpg = code("""
        |var foo1: (Int)
        |var foo2: (Int, Int)
        |""".stripMargin)
      ???
    }

  }

}
