package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class DirectiveTests extends AbstractPassTest {

  "DirectiveTests" should {

    "testSwitchIfConfig" ignore AstFixture("""
      |switch x {
      |  case 1: fallthrough
      |  #if FOO
      |  case 2: fallthrough
      |  case 3: print(3)
      |  case 4: print(4)
      |  #endif
      |  case 5: fallthrough
      |  case 6: print(6)
      |  #if BAR
      |  #if BAZ
      |  case 7: print(7)
      |  case 8: fallthrough
      |  #endif
      |  case 9: fallthrough
      |  #endif
      |  case 10: print(10)
      |}
      |""".stripMargin) { cpg => ??? }

    "testPostfixIfConfigExpression" ignore AstFixture("""
      |foo
      |  .bar()
      |  .baz()
      |  #if CONFIG1
      |  .quux
      |  .garp
      |  #if CONFIG2
      |  .quux
      |  #if CONFIG3
      |    #if INNER1
      |     .quux
      |     .garp
      |    #endif
      |  #elseif CONFIG3
      |  .quux
      |  .garp
      |  #else
      |  .gorp
      |  #endif
      |  .garp
      |  #endif
      |  #endif""".stripMargin) { cpg => ??? }

    "testSourceLocation1" ignore AstFixture("#sourceLocation()") { cpg => ??? }

    "testSourceLocation2" ignore AstFixture("""#sourceLocation(file: "foo", line: 42)""") { cpg => ??? }

    "testHasAttribute" ignore AstFixture("""
      |@frozen
      |#if hasAttribute(foo)
      |@foo
      |#endif
      |public struct S2 { }
      |""".stripMargin) { cpg => ??? }
  }

}
