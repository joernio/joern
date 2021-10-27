package io.shiftleft.console

import io.shiftleft.console.workspacehandling.Project
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HelpTests extends AnyWordSpec with Matchers {

  "Help" should {
    "provide overview of commands as table" in {
      Help.overview[io.shiftleft.console.Console[Project]].contains("| CPG") shouldBe true
    }
  }

}
