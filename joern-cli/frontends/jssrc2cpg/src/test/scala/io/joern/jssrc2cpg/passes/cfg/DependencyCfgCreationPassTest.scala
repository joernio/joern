package io.joern.jssrc2cpg.passes.cfg

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge

class DependencyCfgCreationPassTest extends AbstractCfgPassTest {

  "CFG generation for global builtins" should {
    "be correct for JSON.parse" in CfgFixture("""JSON.parse("foo");""") { implicit cpg =>
      succOf(":program") shouldBe expected((""""foo"""", AlwaysEdge))
      succOf(""""foo"""") shouldBe expected(("""JSON.parse("foo")""", AlwaysEdge))
      succOf("""JSON.parse("foo")""") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for JSON.stringify" in CfgFixture("""JSON.stringify(foo);""") { implicit cpg =>
      succOf(":program") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("JSON.stringify(foo)", AlwaysEdge))
      succOf("JSON.stringify(foo)") shouldBe expected(("RET", AlwaysEdge))
    }
  }

}
