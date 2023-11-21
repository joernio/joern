package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class VariadicGenericsTests extends AbstractPassTest {

  "VariadicGenericsTests" should {

    "testRequirement" ignore AstFixture("func requirement<each T>() where each T: P {}") { cpg => ??? }

    "testPackElementExprSimple" ignore AstFixture("""
        |func tuplify<each T>(_ t: repeat each T) -> (repeat each T) {
        |  return (repeat each t)
        |}
        |func zip<each T, each U>(_ first: repeat each T, with second: repeat each U) -> (repeat (each T, each U)) {
        |  return (repeat (each first, each second))
        |}
        |func variadicMap<each T, each Result>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
        |  return (repeat (each transform)(each t))
        |}
        |""".stripMargin) { cpg => ??? }

    "testMetatype" ignore AstFixture("G<Int, repeat Array<each T>>.self") { cpg => ??? }

    "testParameterPacks1" ignore AstFixture("func f1<each T>() -> repeat each T {}") { cpg => ??? }

    "testParameterPacks2" ignore AstFixture("func f2<each T>() -> (repeat each T) {}") { cpg => ??? }

    "testParameterPacks3" ignore AstFixture("func f3<each T>() -> G<repeat each T> {}") { cpg => ??? }

    "testParameterPacks5" ignore AstFixture("typealias Alias<each T> = (repeat each T)") { cpg => ??? }

    "testParameterPacks6" ignore AstFixture("struct S<each T> {}") { cpg => ??? }

    "testParameterPacks7" ignore AstFixture("struct S<T, each U> {}") { cpg => ??? }

    "testParameterPacks8" ignore AstFixture("struct S<each T, U> {}") { cpg => ??? }

    "testParameterPacks9" ignore AstFixture("struct S<each T:P, U> {}") { cpg => ??? }

    "testParameterPacks10" ignore AstFixture("struct S<each T :P, U> {}") { cpg => ??? }

    "testParameterPacks11" ignore AstFixture("struct S<each T: P> {}") { cpg => ??? }

    "testParameterPacks12" ignore AstFixture("struct S<each T : P> {}") { cpg => ??? }

    "testParameterPacks13" ignore AstFixture("func foo<each T>(_ x: repeat each T) {}") { cpg => ??? }

    "testParameterPacks14" ignore AstFixture("func foo<each T:P>(_ x: repeat each T) {}") { cpg => ??? }

    "testParameterPacks15" ignore AstFixture("func foo<each T :P>(_ x: repeat each T) {}") { cpg => ??? }

    "testParameterPacks16" ignore AstFixture("func foo<each T : P>(_ x: repeat each T) {}") { cpg => ??? }

    "testParameterPacks17" ignore AstFixture("func foo<each T: P>(_ x: repeat each T) {}") { cpg => ??? }

    "testParameterPacks18" ignore AstFixture("func foo<T, U, each V>(x: T, y: U, z: repeat each V) {}") { cpg => ??? }

    "testParameterPacks19" ignore AstFixture("func foo<T, each U, V>(x: T, y: repeat each U, z: V) {}") { cpg => ??? }

    "testParameterPacks20" ignore AstFixture(
      "func foo<each T, each U, each V>(x: repeat each T, y: repeat each U, z: repeat each V) {}"
    ) { cpg => ??? }

    "testParameterPacks21" ignore AstFixture("""
        |enum E<each T> {
        |  case f1(_: repeat each T)
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks22" ignore AstFixture("""
        |enum E<each T> {
        |  case f2(_: G<repeat each T>)
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks23" ignore AstFixture("""
        |enum E<each T> {
        |  var x: repeat each T { fatalError() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks24" ignore AstFixture("""
        |enum E<each T> {
        |  var x: (repeat each T) { fatalError() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks25" ignore AstFixture("""
        |enum E<each T> {
        |  subscript(_: repeat each T) -> Int { fatalError() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks26" ignore AstFixture("""
        |enum E<each T> {
        |  subscript() -> repeat each T { fatalError() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testParameterPacks27" ignore AstFixture("""
        |enum E<each T> {
        |  subscript() -> (repeat each T) { fatalError() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testVariadicTypes" ignore AstFixture("""
        |let _: G< > = G()
        |let _: G<repeat each T> = G()
        |let _: G<Int, repeat each T> = G()
        |let _ = G< >.self
        |let _ = G<repeat each T>.self
        |let _ = G<Int, repeat each T>.self
        |""".stripMargin) { cpg => ??? }

    "testMissingCommaInType" ignore AstFixture("""
        |var foo1: (Int)
        |var foo2: (Int, Int)
        |""".stripMargin) { cpg => ??? }

  }

}
