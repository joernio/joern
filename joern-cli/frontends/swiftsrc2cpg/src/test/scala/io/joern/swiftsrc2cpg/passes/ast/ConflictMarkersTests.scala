// This test file has been translated from swift/test/Parse/conflict_markers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ConflictMarkersTests extends AbstractPassTest {

  "ConflictMarkersTests" should {

    "testConflictMarkers2" ignore AstFixture("""
        |prefix operator <<<<<<<
        |infix operator <<<<<<<
        |""".stripMargin) { cpg => ??? }

    "testConflictMarkers3" ignore AstFixture("""
        |prefix func <<<<<<< (x : String) {}
        |func <<<<<<< (x : String, y : String) {}
        |""".stripMargin) { cpg => ??? }

    "testConflictMarkers4" ignore AstFixture("""
        |prefix operator >>>>>>>
        |infix operator >>>>>>>
        |""".stripMargin) { cpg => ??? }

    "testConflictMarkers5" ignore AstFixture("""
        |prefix func >>>>>>> (x : String) {}
        |func >>>>>>> (x : String, y : String) {}
        |""".stripMargin) { cpg => ??? }

    "testConflictMarkers9" ignore AstFixture("""
        |<<<<<<<"HEAD:fake_conflict_markers.swift" // No error
        |>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift" // No error
        |""".stripMargin) { cpg => ??? }

  }

}
