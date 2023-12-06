// This test file has been translated from swift/test/Parse/switch.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SwitchTests extends AbstractPassTest {
  "SwitchTests" should {

    "testSwitch9" ignore AstFixture(" switch x {}") { cpg => ??? }

    "testSwitch10" ignore AstFixture("""
        |switch x {
        |  case 0:
        |    x = 0
        |  // Multiple patterns per case
        |  case 1, 2, 3:
        |    x = 0
        |  // 'where' guard
        |  case _ where x % 2 == 0:
        |    x = 1
        |    x = 2
        |    x = 3
        |  case _ where x % 2 == 0,
        |   _ where x % 3 == 0:
        |    x = 1
        |  case 10,
        |   _ where x % 3 == 0:
        |    x = 1
        |  case _ where x % 2 == 0,
        |   20:
        |    x = 1
        |  case var y where y % 2 == 0:
        |    x = y + 1
        |  case _ where 0:
        |    x = 0
        |  default:
        |    x = 1
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch11" ignore AstFixture("""
        |// Multiple cases per case block
        |switch x {
        |  case 0:
        |  case 1:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch12" ignore AstFixture("""
        |switch x {
        |  case 0:
        |  default:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch13" ignore AstFixture("""
        |switch x {
        |  case 0:
        |    x = 0
        |  case 1:
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch14" ignore AstFixture("""
        |switch x {
        |  case 0:
        |    x = 0
        |  default:
        |}""".stripMargin) { cpg => ??? }

    "testSwitch17" ignore AstFixture("""
        |switch x {
        |  default:
        |    x = 0
        |  default:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch20" ignore AstFixture("""
        |switch x {
        |  default:
        |  case 0:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch21" ignore AstFixture("""
        |switch x {
        |  default:
        |  default:
        |    x = 0
        |}""".stripMargin) { cpg => ??? }

    "testSwitch23" ignore AstFixture("""
        |switch x {
        |  case 0:
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch24" ignore AstFixture("""
        |switch x {
        |  case 0:
        |  case 1:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch25" ignore AstFixture("""
        |switch x {
        |  case 0:
        |    x = 0
        |  case 1:
        |}""".stripMargin) { cpg => ??? }

    "testSwitch28" ignore AstFixture("""
        |switch x {
        |  case 0:
        |    fallthrough
        |  case 1:
        |    fallthrough
        |  default:
        |    fallthrough
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch29" ignore AstFixture("""
        |// Fallthrough can transfer control anywhere within a case and can appear
        |// multiple times in the same case.
        |switch x {
        |  case 0:
        |    if true { fallthrough }
        |    if false { fallthrough }
        |    x += 1
        |  default:
        |    x += 1
        |}""".stripMargin) { cpg => ??? }

    "testSwitch34" ignore AstFixture("""
        |// Fallthroughs can only transfer control into a case label with bindings if the previous case binds a superset of those vars.
        |switch t {
        |  case (1, 2):
        |    fallthrough
        |  case (var a, var b):
        |    t = (b, a)
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch36" ignore AstFixture("""
        |func patternVarDiffType(x: Int, y: Double) {
        |  switch (x, y) {
        |    case (1, let a):
        |  fallthrough
        |    case (let a, _):
        |  break
        |    }
        |}""".stripMargin) { cpg => ??? }

    "testSwitch38" ignore AstFixture("""
        |func test_label(x : Int) {
        |  Gronk:
        |  switch x {
        |    case 42: return
        |    }
        |  }
        |""".stripMargin) { cpg => ??? }

    "testSwitch42" ignore AstFixture("""
        |switch Whatever.Thing {
        |  case .Thing:
        |  @unknown case _:
        |    x = 0
        |}""".stripMargin) { cpg => ??? }

    "testSwitch43" ignore AstFixture("""
        |switch Whatever.Thing {
        |  case .Thing:
        |  @unknown default:
        |    x = 0
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch54" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case _, _, _:
        |    break
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch55" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case let value:
        |    _ = value
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch56" ignore AstFixture("""
        |switch (Whatever.Thing, Whatever.Thing) {
        |  @unknown case (_, _):
        |    break
        |}
        |""".stripMargin) { cpg => ??? }

    "testSwitch57" ignore AstFixture("""
       |switch Whatever.Thing {
       |  @unknown case is Whatever:
       |    break
       |}
       |""".stripMargin) { cpg => ??? }

    "testSwitch58" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case .Thing:
        |    break
        |}""".stripMargin) { cpg => ??? }

    "testSwitch59" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case (_): // okay
        |    break
        |}""".stripMargin) { cpg => ??? }

    "testSwitch60" ignore AstFixture("""
        |switch Whatever.Thing {
        |  @unknown case _ where x == 0:
        |    break
        |}""".stripMargin) { cpg => ??? }

  }

}
