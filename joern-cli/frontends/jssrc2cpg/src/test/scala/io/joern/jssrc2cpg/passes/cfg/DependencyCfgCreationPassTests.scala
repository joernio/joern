package io.joern.jssrc2cpg.passes.cfg

import io.joern.jssrc2cpg.testfixtures.JsSrcCfgTestCpg
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.testfixtures.CfgTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg

class DependencyCfgCreationPassTests extends CfgTestFixture(() => new JsSrcCfgTestCpg()) {

  "CFG generation for global builtins" should {
    "be correct for JSON.parse" in {
      implicit val cpg: Cpg = code("""JSON.parse("foo");""")
      succOf(":program") should contain theSameElementsAs expected((""""foo"""", AlwaysEdge))
      succOf(""""foo"""") should contain theSameElementsAs expected(("""JSON.parse("foo")""", AlwaysEdge))
      succOf("""JSON.parse("foo")""") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "have correct structure for JSON.stringify" in {
      implicit val cpg: Cpg = code("""JSON.stringify(foo);""")
      succOf(":program") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("JSON.stringify(foo)", AlwaysEdge))
      succOf("JSON.stringify(foo)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }

}
