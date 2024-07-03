package io.shiftleft.semanticcpg

import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OverlaysTests extends AnyWordSpec with Matchers {

  "Overlays" should {

    "allow adding a name to the `overlays` list" in {
      val cpg = MockCpg().withMetaData().cpg
      Overlays.appendOverlayName(cpg, "foo")
      Overlays.appendOverlayName(cpg, "bar")
      cpg.metaData.head.overlays shouldBe List("foo", "bar")
    }

    "allow querying `overlays`" in {
      val cpg = MockCpg().withMetaData().cpg
      Overlays.appendOverlayName(cpg, "foo")
      Overlays.appendOverlayName(cpg, "bar")
      Overlays.appliedOverlays(cpg) shouldBe List("foo", "bar")
    }

    "allow removing last overlay name" in {
      val cpg = MockCpg().withMetaData().cpg
      Overlays.appendOverlayName(cpg, "foo")
      Overlays.appliedOverlays(cpg) shouldBe List("foo")
      Overlays.appendOverlayName(cpg, "bar")
      Overlays.appliedOverlays(cpg) shouldBe List("foo", "bar")
      Overlays.removeLastOverlayName(cpg)
      Overlays.appliedOverlays(cpg) shouldBe List("foo")
      Overlays.removeLastOverlayName(cpg)
      Overlays.appliedOverlays(cpg) shouldBe List()
    }

  }

}
