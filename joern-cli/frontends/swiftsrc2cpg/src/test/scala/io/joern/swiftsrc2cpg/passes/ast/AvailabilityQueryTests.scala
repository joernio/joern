// This test file has been translated from swift/test/Parse/availability_query.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class AvailabilityQueryTests extends AbstractPassTest {

  "AvailabilityQueryTests" should {

    "testAvailabilityQuery1" ignore AstFixture("if #available(OSX 10.51, *) {}") { cpg => }

    "testAvailabilityQuery2" ignore AstFixture("if #unavailable(OSX 10.51, *) {}") { cpg => }

    "testAvailabilityQuery5a" ignore AstFixture("if #unavailable(OSX 10.52, *) {}") { cpg => }

    "testAvailabilityQuery5b" ignore AstFixture("if let _ = Optional(5), #unavailable(OSX 10.52, *) {}") { cpg => }

    "testAvailabilityQuery6" ignore AstFixture("if #available(OSX 10.51, *), #available(OSX 10.52, *) {}") { cpg => }

    "testAvailabilityQuery10" ignore AstFixture("if #available(OSX) {}") { cpg => }

    "testAvailabilityQuery12" ignore AstFixture("if #available(OSX 10.51) {}") { cpg => }

    "testAvailabilityQuery13" ignore AstFixture("if #available(iDishwasherOS 10.51) {}") { cpg => }

    "testAvailabilityQuery14" ignore AstFixture(" if #available(iDishwasherOS 10.51, *) {}") { cpg => }

    "testAvailabilityQuery15" ignore AstFixture(" if #available(macos 10.51, *) {}") { cpg => }

    "testAvailabilityQuery16" ignore AstFixture("if #available(mscos 10.51, *) {}") { cpg => }

    "testAvailabilityQuery17" ignore AstFixture("if #available(macoss 10.51, *) {}") { cpg => }

    "testAvailabilityQuery18" ignore AstFixture("if #available(mac 10.51, *) {}") { cpg => }

    "testAvailabilityQuery19" ignore AstFixture("if #available(OSX 10.51, OSX 10.52, *) {}") { cpg => }

    "testAvailabilityQuery20" ignore AstFixture("if #available(OSX 10.52) { }") { cpg => }

    "testAvailabilityQuery21" ignore AstFixture("if #available(OSX 10.51, iOS 8.0) { }") { cpg => }

    "testAvailabilityQuery22" ignore AstFixture("if #available(iOS 8.0, *) {}") { cpg => }

    "testAvailabilityQuery24" ignore AstFixture("if #available(*) {}") { cpg => }

    "testAvailabilityQuery35" ignore AstFixture("if 1 != 2, #available(iOS 8.0, *) {}") { cpg => }

    "testAvailabilityQuery36" ignore AstFixture("""
      |if case 42 = 42, #available(iOS 8.0, *) {}
      |if let _ = Optional(42), #available(iOS 8.0, *) {}
      |""".stripMargin) { cpg => }

  }

}
