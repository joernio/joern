// This test file has been translated from swift/test/Parse/conflict_markers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class ConflictMarkersTests extends AstSwiftSrc2CpgSuite {

  "ConflictMarkersTests" should {

    "testConflictMarkers2" ignore {
      val cpg = code("""
        |prefix operator <<<<<<<
        |infix operator <<<<<<<
        |""".stripMargin)
      ???
    }

    "testConflictMarkers3" ignore {
      val cpg = code("""
        |prefix func <<<<<<< (x : String) {}
        |func <<<<<<< (x : String, y : String) {}
        |""".stripMargin)
      ???
    }

    "testConflictMarkers4" ignore {
      val cpg = code("""
        |prefix operator >>>>>>>
        |infix operator >>>>>>>
        |""".stripMargin)
      ???
    }

    "testConflictMarkers5" ignore {
      val cpg = code("""
        |prefix func >>>>>>> (x : String) {}
        |func >>>>>>> (x : String, y : String) {}
        |""".stripMargin)
      ???
    }

    "testConflictMarkers9" ignore {
      val cpg = code("""
        |<<<<<<<"HEAD:fake_conflict_markers.swift" // No error
        |>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift" // No error
        |""".stripMargin)
      ???
    }

  }

}
