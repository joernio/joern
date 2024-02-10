package io.joern.console

import io.joern.console.workspacehandling.Project
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HelpTests extends AnyWordSpec with Matchers {
  import testing.availableWidthProvider

  "Help" should {
    "provide overview of commands as table" in {
      Help.overview(classOf[io.joern.console.Console[Project]]) should include("CPG of the active project")
    }
  }

}
