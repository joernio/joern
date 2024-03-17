// This test file has been translated from swift/test/Parse/availability_query.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class AvailabilityQueryTests extends AstSwiftSrc2CpgSuite {

  "AvailabilityQueryTests" should {

    "testAvailabilityQuery1" ignore {
      val cpg = code("if #available(OSX 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery2" ignore {
      val cpg = code("if #unavailable(OSX 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery5a" ignore {
      val cpg = code("if #unavailable(OSX 10.52, *) {}")
      ???
    }

    "testAvailabilityQuery5b" ignore {
      val cpg = code("if let _ = Optional(5), #unavailable(OSX 10.52, *) {}")
      ???
    }

    "testAvailabilityQuery6" ignore {
      val cpg = code("if #available(OSX 10.51, *), #available(OSX 10.52, *) {}")
      ???
    }

    "testAvailabilityQuery10" ignore {
      val cpg = code("if #available(OSX) {}")
      ???
    }

    "testAvailabilityQuery12" ignore {
      val cpg = code("if #available(OSX 10.51) {}")
      ???
    }

    "testAvailabilityQuery13" ignore {
      val cpg = code("if #available(iDishwasherOS 10.51) {}")
      ???
    }

    "testAvailabilityQuery14" ignore {
      val cpg = code(" if #available(iDishwasherOS 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery15" ignore {
      val cpg = code(" if #available(macos 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery16" ignore {
      val cpg = code("if #available(mscos 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery17" ignore {
      val cpg = code("if #available(macoss 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery18" ignore {
      val cpg = code("if #available(mac 10.51, *) {}")
      ???
    }

    "testAvailabilityQuery19" ignore {
      val cpg = code("if #available(OSX 10.51, OSX 10.52, *) {}")
      ???
    }

    "testAvailabilityQuery20" ignore {
      val cpg = code("if #available(OSX 10.52) { }")
      ???
    }

    "testAvailabilityQuery21" ignore {
      val cpg = code("if #available(OSX 10.51, iOS 8.0) { }")
      ???
    }

    "testAvailabilityQuery22" ignore {
      val cpg = code("if #available(iOS 8.0, *) {}")
      ???
    }

    "testAvailabilityQuery24" ignore {
      val cpg = code("if #available(*) {}")
      ???
    }

    "testAvailabilityQuery35" ignore {
      val cpg = code("if 1 != 2, #available(iOS 8.0, *) {}")
      ???
    }

    "testAvailabilityQuery36" ignore {
      val cpg = code("""
      |if case 42 = 42, #available(iOS 8.0, *) {}
      |if let _ = Optional(42), #available(iOS 8.0, *) {}
      |""".stripMargin)
      ???
    }

  }

}
