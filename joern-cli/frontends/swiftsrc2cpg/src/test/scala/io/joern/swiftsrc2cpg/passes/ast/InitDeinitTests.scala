// This test file has been translated from swift/test/Parse/init_deinit.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class InitDeinitTests extends AbstractPassTest {

  "InitDeinitTests" should {

    "testInitDeinit1" ignore AstFixture("""
        |struct FooStructConstructorA {
        |  init()
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit2" ignore AstFixture("""
        |struct FooStructConstructorA {
        |  init() {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit3" ignore AstFixture("""
        |struct FooStructConstructorA {
        |  init<T>() {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit4" ignore AstFixture("""
        |struct FooStructConstructorA {
        |  init?() { self.init() }
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit5" ignore AstFixture("""
        |struct FooStructDeinitializerA {
        |  deinit
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit6" ignore AstFixture("""
        |struct FooStructDeinitializerA {
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit7" ignore AstFixture("""
        |class FooStructDeinitializerA {
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit12" ignore AstFixture("""
        |deinit {}
        |deinit
        |deinit {}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit13" ignore AstFixture("""
        |struct BarStruct {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit14" ignore AstFixture("""
        |extension BarStruct {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit15" ignore AstFixture("""
        |enum BarUnion {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit16" ignore AstFixture("""
        |extension BarUnion {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit17" ignore AstFixture("""
        |class BarClass {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit19" ignore AstFixture("""
        |protocol BarProtocol {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit20" ignore AstFixture("""
        |extension BarProtocol {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit21" ignore AstFixture("""
        |func fooFunc() {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testInitDeinit24" ignore AstFixture("""
        |class Aaron {
        |  convenience init() { init(x: 1) }
        |}
        |""".stripMargin) { cpg => ??? }

    "testDeinitInSwiftinterfaceIsFollowedByFinalFunc" ignore AstFixture("""
        |class Foo {
        |  deinit
        |  final func foo()
        |}
        |""".stripMargin) { cpg => ??? }

    "testDeinitAsync" ignore AstFixture("""
        |class FooClassDeinitializerA {
        |  deinit async {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testAsyncDeinit" ignore AstFixture(
      // This is expected for now.
      // `async` is parsed as a modifier like `public` because you can have an `async var x: Int`.
      """
        |class FooClassDeinitializerA {
        |  async deinit {}
        |}
        |""".stripMargin
    ) { cpg => ??? }

  }

}
